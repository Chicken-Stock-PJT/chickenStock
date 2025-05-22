package com.example.chickenstock.ui.screens.ranking

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import androidx.navigation.NavBackStackEntry
import com.example.chickenstock.R
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.model.DashboardResponse
import androidx.compose.ui.platform.LocalContext
import kotlinx.coroutines.launch
import com.example.chickenstock.ui.screens.mypage.DashboardPortfolioTabContent
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.Refresh
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.tween
import androidx.compose.ui.graphics.graphicsLayer
import com.example.chickenstock.api.TradeHistoryService
import com.example.chickenstock.model.TradeHistoryResponse
import com.example.chickenstock.ui.screens.mypage.TradeHistoryItem
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AIDashboardScreen(
    navController: NavHostController
) {
    val nickname = navController.currentBackStackEntry?.arguments?.getString("nickname")
    val memberIdArg = navController.currentBackStackEntry?.arguments?.getString("memberId")
    val memberId = memberIdArg?.toIntOrNull()
    var selectedTabIndex by remember { mutableStateOf(0) }
    val tabs = listOf("포트폴리오", "거래 기록")

    // 닉네임 -> memberId 매핑 (AI만)
    val aiMemberId = remember(nickname) {
        when (nickname?.trim()?.lowercase()) {
            "귀요미 ai" -> 1
            "쿨한 ai" -> 2
            "chill~ai" -> 3
            "미니 ai" -> 4
            else -> null
        }
    }

    // AI 설명 매핑
    val aiDescription = remember(nickname) {
        when (nickname?.trim()?.lowercase()) {
            "귀요미 ai" -> "엔벨로프(Envelope) 전략:\n엔벨로프 전략은 이동평균선을 기준으로 상단과 하단 밴드를 설정하여 주가의 과열 및 침체 구간을 판단합니다. 주가가 하단 밴드를 터치하면 매수하고, 중앙선이나 상단선에 도달하면 순차적으로 매도합니다. 절반 매도와 전량 매도의 이분화된 구조로 수익 실현 전략을 명확히 합니다. 비교적 단순하지만 강건한 구조로, 추세 반전 구간에서 유용하게 작동합니다."
            "쿨한 ai" -> "단타 매매 전략 (거래량 & 눌림목 기반):\n이 전략은 단기적인 거래량 급증과 가격 눌림목 패턴을 포착해 빠르게 진입/이탈하는 구조입니다. 거래대금 상위 종목 중 유망 종목을 선별하고, 분할 매수·매도를 통해 리스크를 분산합니다. 눌림목 패턴, 기준선 접근, 손절·익절 조건을 복합적으로 활용하여 정밀한 진입/청산 시점을 도출합니다. 빠른 매매 회전율을 기반으로 한 적극적인 단타 전략입니다."
            "chill~ai" -> "볼린저 밴드 전략:\n볼린저 밴드 전략은 가격이 통계적으로 평균에서 얼마나 벗어났는지를 측정하여 매매 타이밍을 포착합니다. 주가가 하단 밴드에 근접하거나 도달하면 매수를, 상단 밴드에 근접하면 매도를 유도합니다. %B 값이 낮을 때 추가 매수 기회를 탐색하고, 급격한 하락 시 손절 조건도 설정되어 있습니다. 안정성과 리스크 관리를 겸비한 전략으로, 변동성이 높은 시장에서도 효과적으로 작동합니다."
            "미니 ai" -> "DRL-UTrans 전략 (강화학습 기반):\nDRL-UTrans 전략은 딥러닝 기반 강화학습(Deep Reinforcement Learning)과 Transformer 구조를 결합한 고도화된 자동매매 모델입니다. 과거 주가 데이터를 시퀀스 형태로 학습하여, 매수·매도·홀딩 중 최적의 액션을 확률 기반으로 판단합니다. 각 종목별 기술적 지표를 스케일링하고, 포트폴리오 상태까지 반영해 맞춤형 의사결정을 수행합니다. 전통적인 룰 기반 전략보다 적응력이 뛰어나며, 시장 변화에 유연하게 대응하는 것이 장점입니다."
            else -> ""
        }
    }

    val context = LocalContext.current
    var dashboardData by remember { mutableStateOf<DashboardResponse?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }
    val coroutineScope = rememberCoroutineScope()

    val rotation = remember { Animatable(0f) }
    var isRefreshing by remember { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

    // 거래 기록 상태
    var tradeHistories by remember { mutableStateOf<List<com.example.chickenstock.model.TradeHistory>>(emptyList()) }
    var tradeIsLoading by remember { mutableStateOf(false) }
    var tradeError by remember { mutableStateOf<String?>(null) }
    var tradeHasNext by remember { mutableStateOf(false) }
    var tradeNextCursor by remember { mutableStateOf<String?>(null) }
    var tradeInitialLoad by remember { mutableStateOf(true) }

    // 포트폴리오 탭 진입 시마다 API 호출
    LaunchedEffect(selectedTabIndex, nickname, memberId) {
        val targetMemberId = aiMemberId ?: memberId
        if (selectedTabIndex == 0 && targetMemberId != null) {
            isLoading = true
            error = null
            dashboardData = null
            try {
                val memberService = RetrofitClient.getInstance(context, ignoreAuthCheck = true).create(MemberService::class.java)
                android.util.Log.d("AIDashboardScreen", "대시보드 API 요청: /members/$targetMemberId/dashboard (memberId=$targetMemberId)")
                val response = memberService.getAIDashboard(targetMemberId)
                if (response.isSuccessful) {
                    dashboardData = response.body()
                } else {
                    error = "대시보드 정보를 불러오지 못했습니다. (${response.code()})"
                }
            } catch (e: Exception) {
                error = e.message ?: "알 수 없는 오류가 발생했습니다."
            } finally {
                isLoading = false
            }
        }
    }

    // 거래 기록 불러오기 함수
    fun loadAiTradeHistories(reset: Boolean = false) {
        val targetMemberId = aiMemberId ?: memberId
        if (targetMemberId == null) return
        tradeIsLoading = true
        tradeError = null
        if (reset) {
            tradeHistories = emptyList()
            tradeNextCursor = null
            tradeHasNext = false
        }
        val pageSize = 20
        val cursor = if (reset) null else tradeNextCursor
        val context = context
        val service = RetrofitClient.getInstance(context, ignoreAuthCheck = true).create(TradeHistoryService::class.java)
        scope.launch {
            try {
                val response = service.getAiTradeHistories(targetMemberId, pageSize, cursor)
                if (response != null) {
                    tradeHistories = if (reset) response.tradeHistories else tradeHistories + response.tradeHistories
                    tradeHasNext = response.hasNext
                    tradeNextCursor = response.nextCursor
                } else {
                    tradeError = "데이터가 없습니다."
                }
            } catch (e: Exception) {
                tradeError = e.message ?: "알 수 없는 오류가 발생했습니다."
            } finally {
                tradeIsLoading = false
                tradeInitialLoad = false
            }
        }
    }

    // 거래 기록 탭 진입 시마다 API 호출
    LaunchedEffect(selectedTabIndex, nickname, memberId) {
        val targetMemberId = aiMemberId ?: memberId
        if (selectedTabIndex == 1 && targetMemberId != null) {
            loadAiTradeHistories(reset = true)
        }
    }

    Scaffold(
        containerColor = Color(0xFFF5F5F5)
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
        ) {
            // 프로필 섹션 (고정)
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .background(
                        color = Color(0xFFFFFBEB),
                        shape = RoundedCornerShape(topStart = 24.dp, topEnd = 24.dp)
                    )
                    .padding(horizontal = 16.dp, vertical = 16.dp)
            ) {
                Column(
                    horizontalAlignment = Alignment.Start,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    Spacer(modifier = Modifier.height(8.dp))
                    // 상단 바: 뒤로가기/새로고침
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        IconButton(
                            onClick = { navController.navigateUp() },
                            modifier = Modifier.size(40.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Filled.KeyboardArrowLeft,
                                contentDescription = "뒤로가기",
                                modifier = Modifier.size(32.dp)
                            )
                        }
                        // 가운데 닉네임 (일반 유저만)
                        if (!nickname.isNullOrBlank() && aiMemberId == null) {
                            Spacer(modifier = Modifier.weight(1f))
                            Text(
                                text = nickname,
                                fontSize = 18.sp,
                                fontWeight = FontWeight.Bold,
                                modifier = Modifier.align(Alignment.CenterVertically)
                            )
                            Spacer(modifier = Modifier.weight(1f))
                        } else {
                            Spacer(modifier = Modifier.weight(1f))
                        }
                        IconButton(
                            onClick = {
                                if (!isRefreshing) {
                                    isRefreshing = true
                                    scope.launch {
                                        rotation.snapTo(0f)
                                        rotation.animateTo(
                                            targetValue = 360f,
                                            animationSpec = tween(durationMillis = 700)
                                        )
                                        val targetMemberId = aiMemberId ?: memberId
                                        if (selectedTabIndex == 0 && targetMemberId != null) {
                                            // 포트폴리오 탭 새로고침
                                            isLoading = true
                                            error = null
                                            dashboardData = null
                                            try {
                                                val memberService = RetrofitClient.getInstance(context, ignoreAuthCheck = true).create(MemberService::class.java)
                                                val response = memberService.getAIDashboard(targetMemberId)
                                                if (response.isSuccessful) {
                                                    dashboardData = response.body()
                                                } else {
                                                    error = "대시보드 정보를 불러오지 못했습니다. (${response.code()})"
                                                }
                                            } catch (e: Exception) {
                                                error = e.message ?: "알 수 없는 오류가 발생했습니다."
                                            } finally {
                                                isLoading = false
                                            }
                                        } else if (selectedTabIndex == 1 && targetMemberId != null) {
                                            // 거래 기록 탭 새로고침
                                            tradeInitialLoad = true
                                            loadAiTradeHistories(reset = true)
                                        }
                                        isRefreshing = false
                                    }
                                }
                            },
                            enabled = !isRefreshing,
                            modifier = Modifier.size(40.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Filled.Refresh,
                                contentDescription = "새로고침",
                                tint = if (isRefreshing) Color.Gray else Color.Black,
                                modifier = Modifier.graphicsLayer { rotationZ = rotation.value }
                            )
                        }
                    }
                    Spacer(modifier = Modifier.height(8.dp))
                    
                    // AI 닉네임/설명 (AI만)
                    if (!nickname.isNullOrBlank() && aiMemberId != null) {
                        Text(
                            text = nickname,
                            fontSize = 18.sp,
                            fontWeight = FontWeight.Bold
                        )
                        Spacer(modifier = Modifier.height(8.dp))
                        // AI 설명
                        if (nickname.trim().lowercase() == ".") {
                            Text(
                                text = aiDescription,
                                fontSize = 14.sp,
                                color = Color.DarkGray,
                                textAlign = TextAlign.Start
                            )
                        } else {
                            // '~ 전략:' 부분만 강조
                            val splitIdx = aiDescription.indexOf(":")
                            if (splitIdx > 0) {
                                val title = aiDescription.substring(0, splitIdx + 1)
                                val desc = aiDescription.substring(splitIdx + 1).trimStart('\n', ' ')
                                Text(
                                    text = title,
                                    fontSize = 16.sp,
                                    fontWeight = FontWeight.Bold,
                                    color = Color.DarkGray,
                                    textAlign = TextAlign.Start
                                )
                                Spacer(modifier = Modifier.height(2.dp))
                                Text(
                                    text = desc,
                                    fontSize = 14.sp,
                                    color = Color.DarkGray,
                                    textAlign = TextAlign.Start
                                )
                            } else {
                                Text(
                                    text = aiDescription,
                                    fontSize = 14.sp,
                                    color = Color.DarkGray,
                                    textAlign = TextAlign.Start
                                )
                            }
                        }
                    }
                    // AI 설명
                }
            }
            // 탭 섹션 (고정)
            Row(
                modifier = Modifier.fillMaxWidth()
            ) {
                tabs.forEachIndexed { index, title ->
                    Column(
                        horizontalAlignment = Alignment.CenterHorizontally,
                        verticalArrangement = Arrangement.Center,
                        modifier = Modifier
                            .weight(1f)
                            .height(48.dp)
                            .background(if (index == selectedTabIndex) Color.White else Color.Transparent)
                            .clickable { selectedTabIndex = index }
                    ) {
                        Box(
                            modifier = Modifier
                                .fillMaxWidth()
                                .weight(1f),
                            contentAlignment = Alignment.Center
                        ) {
                            Text(
                                text = title,
                                fontSize = 16.sp,
                                fontWeight = if (index == selectedTabIndex) FontWeight.Bold else FontWeight.Normal,
                                color = if (index == selectedTabIndex) Color.Black else Color.Gray,
                                textAlign = TextAlign.Center
                            )
                        }
                        Box(
                            modifier = Modifier
                                .height(3.dp)
                                .fillMaxWidth()
                                .background(if (index == selectedTabIndex) Color(0xFFFFEB3B) else Color.Transparent)
                        )
                    }
                }
            }
            // 탭 내용
            when (selectedTabIndex) {
                0 -> {
                    // 포트폴리오 탭 (API 데이터)
                    when {
                        isLoading -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { CircularProgressIndicator(color = Color(0xFFFFEB3B)) }
                        error != null -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { Text(error!!, color = Color.Red) }
                        dashboardData != null -> DashboardPortfolioTabContent(dashboardData = dashboardData, navController = navController)
                        else -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { Text("데이터가 없습니다.", color = Color.Gray) }
                    }
                }
                1 -> {
                    // 거래 기록 탭 (AI 체결 내역)
                    when {
                        tradeIsLoading && tradeInitialLoad -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { CircularProgressIndicator(color = Color(0xFFFFEB3B)) }
                        tradeError != null -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { Text(tradeError!!, color = Color.Red) }
                        tradeHistories.isEmpty() -> Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) { Text("거래 내역이 없습니다.", color = Color.Gray) }
                        else -> LazyColumn(
                            modifier = Modifier.fillMaxWidth(),
                            contentPadding = PaddingValues(16.dp)
                        ) {
                            if (tradeHistories.isEmpty()) {
                                item {
                                    Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                                        Text("거래 내역이 없습니다.", color = Color.Gray)
                                    }
                                }
                            } else {
                                items(tradeHistories.size) { index ->
                                    TradeHistoryItem(history = tradeHistories[index])
                                    if (index == tradeHistories.size - 1 && tradeHasNext) {
                                        LaunchedEffect(tradeNextCursor) {
                                            if (!tradeIsLoading) loadAiTradeHistories()
                                        }
                                        Box(
                                            modifier = Modifier
                                                .fillMaxWidth()
                                                .padding(vertical = 16.dp),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            CircularProgressIndicator(
                                                modifier = Modifier.size(24.dp),
                                                color = Color(0xFFFFEB3B),
                                                strokeWidth = 2.dp
                                            )
                                        }
                                    } else if (index != tradeHistories.size - 1) {
                                        Spacer(modifier = Modifier.height(12.dp))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
} 