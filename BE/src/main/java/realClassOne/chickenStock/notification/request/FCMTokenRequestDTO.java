package realClassOne.chickenStock.notification.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class FCMTokenRequestDTO {
    private String token;
}