package realClassOne.chickenStock.member.dto.request;

import lombok.Getter;

@Getter
public class PasswordChangeRequestDTO {
    private String currentPassword;
    private String newPassword;
    private String checkPassword;

    public static PasswordChangeRequestDTO of(String currentPassword, String newPassword, String checkPassword) {
        PasswordChangeRequestDTO dto = new PasswordChangeRequestDTO();
        dto.currentPassword = currentPassword;
        dto.newPassword = newPassword;
        dto.checkPassword = checkPassword;
        return dto;
    }
}
