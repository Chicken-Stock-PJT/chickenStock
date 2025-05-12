package realClassOne.chickenStock.member.dto.response;

import lombok.Getter;

@Getter
public class NicknameChangeResponseDTO {

    private final String message;

    // private 생성자
    private NicknameChangeResponseDTO(String message) {
        this.message = message;
    }

    // 정적 팩토리 메서드
    public static NicknameChangeResponseDTO of(String message) {
        return new NicknameChangeResponseDTO(message);
    }
}