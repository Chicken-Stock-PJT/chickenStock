package realClassOne.chickenStock.chat.dto;

import lombok.Data;

@Data
public class NotificationMessage {
    private String type;
    private String title;
    private String message;
    private Long timestamp;
    private Long notificationId;
}