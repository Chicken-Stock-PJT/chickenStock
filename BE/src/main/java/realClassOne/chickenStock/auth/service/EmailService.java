package realClassOne.chickenStock.auth.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import realClassOne.chickenStock.auth.dto.response.EmailVerifyResponseDTO;
import realClassOne.chickenStock.auth.repository.VerificationCodeRepository;

import java.util.Random;
import java.util.concurrent.TimeUnit;


@Service
@RequiredArgsConstructor
public class EmailService {

    private final JavaMailSender mailSender;
    private final RedisTemplate<String, String> redisTemplate;
    private final VerificationCodeRepository verificationCodeRepository;

    @Value("${spring.mail.username}")
    private String fromAddress;

    private static final String EMAIL_CODE_PREFIX = "email:code:";
    private static final long CODE_TTL_SECONDS = 300L; // 5분 TTL

    public void sendVerificationCodeWithTTL(String email) {
        String code = generateCode();
        String redisKey = EMAIL_CODE_PREFIX + email;

        redisTemplate.opsForValue().set(redisKey, code, CODE_TTL_SECONDS, TimeUnit.SECONDS);

        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(fromAddress);
        message.setTo(email);
        message.setSubject("[치킨스톡] 이메일 인증 코드 안내드립니다");
        message.setText("""
            안녕하세요, 치킨스톡 회원님!
            
            요청하신 이메일 인증 코드는 아래와 같습니다:
            
            인증 코드:""" + code + """
            
            ※ 본 인증 코드는 5분간 유효하며, 타인에게 공유하지 마세요.
            감사합니다.
            """);


        mailSender.send(message);
    }

    public EmailVerifyResponseDTO verifyEmailCodeAndRespond(String email, String code) {
        String redisKey = EMAIL_CODE_PREFIX + email;
        String storedCode = redisTemplate.opsForValue().get(redisKey);
        if (storedCode == null) {
            return EmailVerifyResponseDTO.of(false, "인증 코드가 만료되었거나 존재하지 않습니다.");
        }

        if (!storedCode.equals(code)) {
            return EmailVerifyResponseDTO.of(false, "인증 코드가 일치하지 않습니다.");
        }

        redisTemplate.delete(redisKey);
        verificationCodeRepository.markVerified(email);
        return EmailVerifyResponseDTO.of(true, "인증에 성공했습니다.");
    }


    private String generateCode() {
        Random random = new Random();
        int code = 100000 + random.nextInt(900000); // 6자리 숫자
        return String.valueOf(code);
    }

    public void sendTemporaryPassword(String email, String tempPassword) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(fromAddress);
        message.setTo(email);
        message.setSubject("[치킨스톡] 임시 비밀번호 안내드립니다");
        message.setText("""
        안녕하세요, 치킨스톡 회원님!

        임시 비밀번호는 아래와 같습니다:

        임시 비밀번호:""" + tempPassword + """
        

        ※ 로그인 후 반드시 비밀번호를 변경해 주세요.
        ※ 타인에게 공유하지 마시고, 보안을 위해 빠른 시일 내 변경을 권장합니다.

        감사합니다.
        """);

        mailSender.send(message);
    }
}
