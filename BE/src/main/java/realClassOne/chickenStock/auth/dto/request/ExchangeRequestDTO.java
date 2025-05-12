package realClassOne.chickenStock.auth.dto.request;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ExchangeRequestDTO {

    private String oneTimeCode;
    private String platform; // "mobile" 또는 "web"으로 받기
}
