"""
DRL-UTrans 네트워크 모델 정의
트랜스포머와 U-Net을 결합한 강화학습 모델

app.models.drl_utrans.network 모듈 구현
참고: 상용 환경에서 사용하기 위해 최적화된 버전
"""
import math
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.distributions import Categorical

class PositionalEncoding(nn.Module):
    """
    트랜스포머를 위한 위치 인코딩
    
    Args:
        d_model (int): 모델의 차원
        max_len (int): 최대 시퀀스 길이
        dropout (float): 드롭아웃 비율
    """
    def __init__(self, d_model, max_len=100, dropout=0.1):
        super(PositionalEncoding, self).__init__()
        self.dropout = nn.Dropout(p=dropout)

        # 위치 인코딩 행렬 계산
        pe = torch.zeros(max_len, d_model)
        position = torch.arange(0, max_len, dtype=torch.float).unsqueeze(1)
        div_term = torch.exp(torch.arange(0, d_model, 2).float() * (-math.log(10000.0) / d_model))
        
        # 사인/코사인 함수를 사용한 위치 인코딩
        pe[:, 0::2] = torch.sin(position * div_term)
        pe[:, 1::2] = torch.cos(position * div_term)
        
        # 차원 변환하여 저장
        pe = pe.unsqueeze(0)
        
        # 버퍼로 등록 (모델 파라미터는 아니지만 저장됨)
        self.register_buffer('pe', pe)

    def forward(self, x):
        """
        Args:
            x: [batch_size, seq_len, d_model] 형태의 입력 텐서
        
        Returns:
            위치 인코딩이 추가된 입력 텐서
        """
        x = x + self.pe[:, :x.size(1), :]
        return self.dropout(x)


class AttentionBlock(nn.Module):
    """
    멀티헤드 셀프 어텐션 블록
    
    Args:
        d_model (int): 모델의 차원
        nhead (int): 어텐션 헤드 수
        dropout (float): 드롭아웃 비율
    """
    def __init__(self, d_model, nhead, dropout=0.1):
        super(AttentionBlock, self).__init__()
        self.self_attn = nn.MultiheadAttention(d_model, nhead, dropout=dropout, batch_first=True)
        self.norm1 = nn.LayerNorm(d_model)
        self.dropout = nn.Dropout(dropout)
    
    def forward(self, x, mask=None):
        """
        Args:
            x: [batch_size, seq_len, d_model] 형태의 입력 텐서
            mask: 어텐션 마스크 (선택 사항)
        
        Returns:
            어텐션이 적용된 텐서
        """
        # 셀프 어텐션 적용
        attn_output, _ = self.self_attn(x, x, x, attn_mask=mask)
        
        # 잔차 연결 및 정규화
        x = x + self.dropout(attn_output)
        x = self.norm1(x)
        
        return x


class FeedForwardBlock(nn.Module):
    """
    피드포워드 네트워크 블록
    
    Args:
        d_model (int): 모델의 차원
        d_ff (int): 피드포워드 네트워크의 내부 차원
        dropout (float): 드롭아웃 비율
    """
    def __init__(self, d_model, d_ff, dropout=0.1):
        super(FeedForwardBlock, self).__init__()
        self.linear1 = nn.Linear(d_model, d_ff)
        self.dropout = nn.Dropout(dropout)
        self.linear2 = nn.Linear(d_ff, d_model)
        self.norm = nn.LayerNorm(d_model)
    
    def forward(self, x):
        """
        Args:
            x: [batch_size, seq_len, d_model] 형태의 입력 텐서
        
        Returns:
            피드포워드 네트워크가 적용된 텐서
        """
        # 피드포워드 네트워크 적용
        ff_output = self.linear2(self.dropout(F.relu(self.linear1(x))))
        
        # 잔차 연결 및 정규화
        x = x + self.dropout(ff_output)
        x = self.norm(x)
        
        return x


class TransformerEncoderLayer(nn.Module):
    """
    트랜스포머 인코더 레이어
    
    Args:
        d_model (int): 모델의 차원
        nhead (int): 어텐션 헤드 수
        d_ff (int): 피드포워드 네트워크의 내부 차원
        dropout (float): 드롭아웃 비율
    """
    def __init__(self, d_model, nhead, d_ff, dropout=0.1):
        super(TransformerEncoderLayer, self).__init__()
        self.attn_block = AttentionBlock(d_model, nhead, dropout)
        self.ff_block = FeedForwardBlock(d_model, d_ff, dropout)
    
    def forward(self, x, mask=None):
        """
        Args:
            x: [batch_size, seq_len, d_model] 형태의 입력 텐서
            mask: 어텐션 마스크 (선택 사항)
        
        Returns:
            트랜스포머 인코더 레이어가 적용된 텐서
        """
        x = self.attn_block(x, mask)
        x = self.ff_block(x)
        return x


class UTransBlock(nn.Module):
    """
    U-Net과 같은 구조의 블록
    
    Args:
        in_channels (int): 입력 채널 수
        out_channels (int): 출력 채널 수
    """
    def __init__(self, in_channels, out_channels):
        super(UTransBlock, self).__init__()
        self.conv = nn.Conv1d(in_channels, out_channels, kernel_size=3, padding=1)
        self.bn = nn.BatchNorm1d(out_channels)
        self.relu = nn.ReLU(inplace=True)
    
    def forward(self, x):
        """
        Args:
            x: [batch_size, in_channels, seq_len] 형태의 입력 텐서
        
        Returns:
            U-Net 블록이 적용된 텐서, 형태: [batch_size, out_channels, seq_len]
        """
        return self.relu(self.bn(self.conv(x)))


class DRLUTransNetwork(nn.Module):
    """
    DRL-UTrans 네트워크: 트랜스포머와 U-Net을 결합한 강화학습 모델
    
    Args:
        input_dim (int): 입력 차원 (특성 수)
        seq_len (int): 시퀀스 길이
        d_model (int): 모델의 내부 차원
        nhead (int): 어텐션 헤드 수
        num_layers (int): 트랜스포머 레이어 수
        d_ff (int): 피드포워드 네트워크의 내부 차원
        dropout (float): 드롭아웃 비율
        action_dim (int): 행동 공간의 차원
    """
    def __init__(self, input_dim, seq_len, d_model=64, nhead=4, num_layers=2, 
                 d_ff=256, dropout=0.1, action_dim=3):
        super(DRLUTransNetwork, self).__init__()
        
        # 입력 임베딩 레이어
        self.embedding = nn.Linear(input_dim, d_model)
        
        # 위치 인코딩
        self.pos_encoder = PositionalEncoding(d_model, max_len=seq_len, dropout=dropout)
        
        # 트랜스포머 인코더 레이어
        self.transformer_layers = nn.ModuleList([
            TransformerEncoderLayer(d_model, nhead, d_ff, dropout)
            for _ in range(num_layers)
        ])
        
        # U-Net 구조 (다운샘플링)
        self.unet_down1 = UTransBlock(d_model, d_model * 2)
        self.unet_down2 = UTransBlock(d_model * 2, d_model * 4)
        self.unet_down3 = UTransBlock(d_model * 4, d_model * 8)
        
        # U-Net 구조 (업샘플링)
        self.unet_up1 = nn.ConvTranspose1d(d_model * 8, d_model * 4, kernel_size=3, padding=1)
        self.unet_up2 = nn.ConvTranspose1d(d_model * 4, d_model * 2, kernel_size=3, padding=1)
        self.unet_up3 = nn.ConvTranspose1d(d_model * 2, d_model, kernel_size=3, padding=1)
        
        # 배치 정규화
        self.bn_up1 = nn.BatchNorm1d(d_model * 4)
        self.bn_up2 = nn.BatchNorm1d(d_model * 2)
        self.bn_up3 = nn.BatchNorm1d(d_model)
        
        # 행동 선택 헤드 (Actor)
        self.action_head = nn.Sequential(
            nn.Linear(d_model, 64),
            nn.ReLU(),
            nn.Linear(64, action_dim)
        )
        
        # 행동 가중치 헤드 (매매 비중)
        self.weight_head = nn.Sequential(
            nn.Linear(d_model, 64),
            nn.ReLU(),
            nn.Linear(64, 1),
            nn.Sigmoid()  # 0~1 사이 매매 비중
        )
        
        # 가치 함수 헤드 (Critic)
        self.value_head = nn.Sequential(
            nn.Linear(d_model, 64),
            nn.ReLU(),
            nn.Linear(64, 1)
        )
        
        # 데이터 차원을 저장
        self.d_model = d_model
        self.seq_len = seq_len
        
    def forward(self, x):
        """
        Args:
            x: [batch_size, seq_len, input_dim] 형태의 입력 텐서
        
        Returns:
            tuple: (action_probs, action_weight, state_value)
            - action_probs: 행동 확률 분포
            - action_weight: 행동 가중치 (매매 비중)
            - state_value: 상태 가치 함수
        """
        batch_size = x.size(0)
        
        # 입력 임베딩 및 위치 인코딩
        x = self.embedding(x)
        x = self.pos_encoder(x)
        
        # 트랜스포머 인코더 레이어 통과
        for layer in self.transformer_layers:
            x = layer(x)
        
        # U-Net 구조를 위한 차원 변환 [batch, seq_len, d_model] -> [batch, d_model, seq_len]
        x_conv = x.transpose(1, 2)
        
        # U-Net 다운샘플링
        down1 = self.unet_down1(x_conv)
        down2 = self.unet_down2(down1)
        down3 = self.unet_down3(down2)
        
        # U-Net 업샘플링 (스킵 연결)
        up1 = F.relu(self.bn_up1(self.unet_up1(down3)))
        up1 = up1 + down2  # 스킵 연결
        
        up2 = F.relu(self.bn_up2(self.unet_up2(up1)))
        up2 = up2 + down1  # 스킵 연결
        
        up3 = F.relu(self.bn_up3(self.unet_up3(up2)))
        up3 = up3 + x_conv  # 스킵 연결
        
        # 다시 원래 차원으로 변환 [batch, d_model, seq_len] -> [batch, seq_len, d_model]
        features = up3.transpose(1, 2)
        
        # 마지막 시퀀스의 특징만 사용
        last_features = features[:, -1, :]
        
        # 행동 확률, 행동 가중치, 상태 가치 계산
        action_probs = F.softmax(self.action_head(last_features), dim=1)
        action_weight = self.weight_head(last_features)
        state_value = self.value_head(last_features)
        
        return action_probs, action_weight, state_value
    
    def act(self, state, deterministic=False):
        """
        주어진 상태에서 행동을 선택
        
        Args:
            state: [1, seq_len, input_dim] 형태의 상태 텐서
            deterministic (bool): 결정론적 행동 선택 여부
        
        Returns:
            tuple: (action, action_weight, log_prob, value)
            - action: 선택된 행동
            - action_weight: 행동 가중치 (매매 비중)
            - log_prob: 행동의 로그 확률
            - value: 상태 가치
        """
        with torch.no_grad():
            # 정책 네트워크로부터 출력 계산
            action_probs, action_weight, state_value = self.forward(state)
            
            # 행동 선택 (확률적 또는 결정론적)
            if deterministic:
                action = torch.argmax(action_probs, dim=1)
            else:
                m = Categorical(action_probs)
                action = m.sample()
            
            # 로그 확률 계산
            log_prob = torch.log(action_probs.squeeze(0)[action.item()] + 1e-10)
            
            return action.item(), action_weight.item(), log_prob.item(), state_value.item()


class DRLUTransActorCritic(nn.Module):
    """
    Actor-Critic 구조의 DRL-UTrans 모델
    
    Args:
        input_dim (int): 입력 차원 (특성 수)
        seq_len (int): 시퀀스 길이
        d_model (int): 모델의 내부 차원
        nhead (int): 어텐션 헤드 수
        num_layers (int): 트랜스포머 레이어 수
        d_ff (int): 피드포워드 네트워크의 내부 차원
        dropout (float): 드롭아웃 비율
        action_dim (int): 행동 공간의 차원
    """
    def __init__(self, input_dim, seq_len, d_model=64, nhead=4, num_layers=2, 
                 d_ff=256, dropout=0.1, action_dim=3):
        super(DRLUTransActorCritic, self).__init__()
        
        # Actor와 Critic이 특성 추출 네트워크를 공유
        self.backbone = DRLUTransNetwork(
            input_dim=input_dim,
            seq_len=seq_len,
            d_model=d_model,
            nhead=nhead,
            num_layers=num_layers,
            d_ff=d_ff,
            dropout=dropout,
            action_dim=action_dim
        )
        
    def forward(self, x):
        """
        Args:
            x: [batch_size, seq_len, input_dim] 형태의 입력 텐서
        
        Returns:
            tuple: (action_probs, action_weight, state_value)
            - action_probs: 행동 확률 분포
            - action_weight: 행동 가중치 (매매 비중)
            - state_value: 상태 가치 함수
        """
        return self.backbone(x)
    
    def act(self, state, deterministic=False):
        """
        주어진 상태에서 행동을 선택
        
        Args:
            state: [1, seq_len, input_dim] 형태의 상태 텐서
            deterministic (bool): 결정론적 행동 선택 여부
        
        Returns:
            tuple: (action, action_weight, log_prob, value)
            - action: 선택된 행동
            - action_weight: 행동 가중치 (매매 비중)
            - log_prob: 행동의 로그 확률
            - value: 상태 가치
        """
        return self.backbone.act(state, deterministic)


class DRLUTransPPOnet(nn.Module):
    """
    PPO 알고리즘을 위한 DRL-UTrans 네트워크
    
    Args:
        input_dim (int): 입력 차원 (특성 수)
        seq_len (int): 시퀀스 길이
        action_dim (int): 행동 공간의 차원
        d_model (int): 모델의 내부 차원
        nhead (int): 어텐션 헤드 수
        num_layers (int): 트랜스포머 레이어 수
        d_ff (int): 피드포워드 네트워크의 내부 차원
        dropout (float): 드롭아웃 비율
    """
    def __init__(self, input_dim, seq_len, action_dim=3, d_model=64, nhead=4, 
                 num_layers=2, d_ff=256, dropout=0.1):
        super(DRLUTransPPOnet, self).__init__()
        
        # 공통 백본 네트워크
        self.actor_critic = DRLUTransActorCritic(
            input_dim=input_dim,
            seq_len=seq_len,
            d_model=d_model,
            nhead=nhead,
            num_layers=num_layers,
            d_ff=d_ff,
            dropout=dropout,
            action_dim=action_dim
        )
        
        # 모델 설정 저장
        self.input_dim = input_dim
        self.seq_len = seq_len
        self.action_dim = action_dim
        
    def forward(self, x):
        """
        Args:
            x: [batch_size, seq_len, input_dim] 형태의 입력 텐서
        
        Returns:
            tuple: (action_probs, action_weight, state_value)
            - action_probs: 행동 확률 분포
            - action_weight: 행동 가중치 (매매 비중)
            - state_value: 상태 가치 함수
        """
        return self.actor_critic(x)
    
    def get_action(self, state, deterministic=False):
        """
        주어진 상태에서 행동을 선택
        
        Args:
            state: [seq_len, input_dim] 형태의 상태 (단일 배치)
            deterministic (bool): 결정론적 행동 선택 여부
        
        Returns:
            tuple: (action, action_weight, log_prob, entropy, value)
            - action: 선택된 행동
            - action_weight: 행동 가중치 (매매 비중)
            - log_prob: 행동의 로그 확률
            - entropy: 엔트로피
            - value: 상태 가치
        """
        # 상태를 배치 차원이 있는 형태로 변환
        if len(state.shape) == 2:
            state = state.unsqueeze(0)
        
        with torch.no_grad():
            # 정책 네트워크로부터 출력 계산
            action_probs, action_weight, state_value = self.forward(state)
            
            # 행동 분포
            dist = Categorical(action_probs)
            
            # 행동 선택 (확률적 또는 결정론적)
            if deterministic:
                action = torch.argmax(action_probs, dim=1)
            else:
                action = dist.sample()
            
            # 로그 확률과 엔트로피 계산
            log_prob = dist.log_prob(action)
            entropy = dist.entropy()
            
            return action.item(), action_weight.item(), log_prob.item(), entropy.item(), state_value.item()
    
    def evaluate(self, states, actions):
        """
        주어진 상태와 행동에 대한 평가 함수
        
        Args:
            states: [batch_size, seq_len, input_dim] 형태의 상태 텐서
            actions: [batch_size] 형태의 행동 텐서
        
        Returns:
            tuple: (log_probs, state_values, entropies, action_weights)
            - log_probs: 행동의 로그 확률
            - state_values: 상태 가치
            - entropies: 엔트로피
            - action_weights: 행동 가중치 (매매 비중)
        """
        # 정책 네트워크로부터 출력 계산
        action_probs, action_weights, state_values = self.forward(states)
        
        # 행동 분포
        dist = Categorical(action_probs)
        
        # 로그 확률과 엔트로피 계산
        log_probs = dist.log_prob(actions)
        entropies = dist.entropy()
        
        return log_probs, state_values.squeeze(), entropies, action_weights.squeeze()

# 모델 타입 정의 (실제 환경에서 사용하기 위함)
MODEL_TYPE = {
    "ppo": DRLUTransPPOnet,
    "actor_critic": DRLUTransActorCritic,
    "backbone": DRLUTransNetwork
}

def load_model(model_path, device=None, model_type="ppo"):
    """
    저장된 모델 로드 유틸리티 함수
    
    Args:
        model_path (str): 모델 파일 경로
        device (torch.device, optional): 모델을 로드할 디바이스
        model_type (str): 모델 타입 ("ppo", "actor_critic", "backbone")
    
    Returns:
        model: 로드된 모델
    """
    import os
    import torch
    
    if device is None:
        device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    
    if not os.path.exists(model_path):
        raise FileNotFoundError(f"모델 파일을 찾을 수 없습니다: {model_path}")
    
    try:
        checkpoint = torch.load(model_path, map_location=device)
        
        # 모델 설정 가져오기
        model_config = checkpoint.get('model_config', {})
        input_dim = model_config.get('input_dim', 22)
        seq_len = model_config.get('seq_len', 20)
        action_dim = model_config.get('action_dim', 3)
        
        # 모델 생성
        model_class = MODEL_TYPE[model_type.lower()]
        model = model_class(
            input_dim=input_dim,
            seq_len=seq_len,
            action_dim=action_dim,
            d_model=64,
            nhead=4,
            num_layers=2,
            d_ff=256,
            dropout=0.1
        ).to(device)
        
        # 모델 가중치 로드
        model.load_state_dict(checkpoint['model_state_dict'])
        model.eval()  # 평가 모드로 설정
        
        return model
    except Exception as e:
        raise RuntimeError(f"모델 로드 중 오류 발생: {str(e)}")