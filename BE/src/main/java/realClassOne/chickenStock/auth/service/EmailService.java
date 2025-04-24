package realClassOne.chickenStock.auth.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.auth.dto.response.EmailVerifyResponseDTO;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

@Service
@RequiredArgsConstructor
public class EmailService {

    private final JavaMailSender mailSender;

    // 인증번호 저장소 (나중에 Redis로 바꾸면 더 좋아요)
    private final Map<String, String> authCodeStorage = new HashMap<>();

    @Value("${spring.mail.username}")
    private String fromAddress;

    public void sendVerificationCode(String email) {
        String code = generateCode();
        authCodeStorage.put(email, code);

        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(fromAddress);
        message.setTo(email);
        message.setSubject("[치킨스톡] 이메일 인증 코드 안내드립니다");
        message.setText("""
            안녕하세요, 치킨스톡 회원님!
            
            요청하신 이메일 인증 코드는 아래와 같습니다:
            
            인증 코드: """ + code + """
            
            ※ 본 인증 코드는 5분간 유효하며, 타인에게 공유하지 마세요.
            감사합니다.
            """);


        mailSender.send(message);
    }

    public boolean verifyCode(String email, String code) {
        String storedCode = authCodeStorage.get(email);
        return code.equals(storedCode);
    }

    public EmailVerifyResponseDTO verifyEmailCodeAndRespond(String email, String code) {
        boolean isValid = verifyCode(email, code);
        if (isValid) {
            return EmailVerifyResponseDTO.of(true, "인증에 성공했습니다.");
        } else {
            return EmailVerifyResponseDTO.of(false, "인증번호가 일치하지 않거나 만료되었습니다.");
        }
    }

    private String generateCode() {
        Random random = new Random();
        int code = 100000 + random.nextInt(900000); // 6자리 숫자
        return String.valueOf(code);
    }


}
