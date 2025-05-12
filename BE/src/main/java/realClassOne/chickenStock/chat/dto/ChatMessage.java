package realClassOne.chickenStock.chat.dto;

import lombok.Data;

@Data
public class ChatMessage {
    private Long memberId;
    private String nickname;
    private String message;
    private Long timestamp;
}