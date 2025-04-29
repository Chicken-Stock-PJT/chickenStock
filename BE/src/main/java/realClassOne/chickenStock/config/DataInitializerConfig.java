package realClassOne.chickenStock.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.password.PasswordEncoder;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.entity.MemberRole;
import realClassOne.chickenStock.member.repository.MemberRepository;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

@Configuration
@RequiredArgsConstructor
public class DataInitializerConfig {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;

    @Bean
    public CommandLineRunner initializeData() {
        return args -> {
            // 이미 존재하는지 확인
            Optional<Member> existingMember = memberRepository.findByEmail("cuteai@gmail.com");

            // 존재하지 않는 경우에만 생성
            if (existingMember.isEmpty()) {
                Set<MemberRole> roles = new HashSet<>();
                roles.add(MemberRole.ROLE_AI);

                Member aiMember = Member.of(
                        "cuteai@gmail.com",
                        passwordEncoder.encode("ssafy123!@#"),
                        "귀요미AI",
                        "귀요미AI",
                        null, // 기본 프로필 이미지 사용
                        "local",
                        "local",
                        roles
                );

                memberRepository.save(aiMember);
                System.out.println("AI 멤버가 성공적으로 생성되었습니다.");
            } else {
                System.out.println("AI 멤버가 이미 존재합니다.");
            }
        };
    }
}