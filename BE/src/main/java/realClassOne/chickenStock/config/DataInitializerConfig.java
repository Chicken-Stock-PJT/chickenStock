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
            // a@a 계정 확인 및 생성
            Optional<Member> existingMemberA = memberRepository.findByEmail("cuteai@gmail.com");

            // 존재하지 않는 경우에만 생성
            if (existingMemberA.isEmpty()) {
                Set<MemberRole> roles = new HashSet<>();
                roles.add(MemberRole.ROLE_AI);

                Member aiMember = Member.of(
                        "cuteai@gmail.com",
                        passwordEncoder.encode("a"),
                        "귀요미 AI",
                        "귀요미 AI",
                        null, // 기본 프로필 이미지 사용
                        "local",
                        null,
                        roles
                );

                memberRepository.save(aiMember);
                System.out.println("AI 멤버가 성공적으로 생성되었습니다.");
            } else {
                System.out.println("AI 멤버가 이미 존재합니다.");
            }

            // b@b 계정 확인 및 생성
            Optional<Member> existingMemberB = memberRepository.findByEmail("coolai@gmail.com");

            // 존재하지 않는 경우에만 생성
            if (existingMemberB.isEmpty()) {
                Set<MemberRole> roles = new HashSet<>();
                roles.add(MemberRole.ROLE_USER); // 일반 사용자 역할 부여

                Member userMember = Member.of(
                        "coolai@gmail.com",
                        passwordEncoder.encode("a"),
                        "쿨한 AI",
                        "쿨한 AI",
                        null, // 기본 프로필 이미지 사용
                        "local",
                        null,
                        roles
                );

                memberRepository.save(userMember);
                System.out.println("AI 멤버가 성공적으로 생성되었습니다.");
            } else {
                System.out.println("AI 멤버가 이미 존재합니다.");
            }

            // b@b 계정 확인 및 생성
            Optional<Member> existingMemberC = memberRepository.findByEmail("chillai@gmail.com");

            // 존재하지 않는 경우에만 생성
            if (existingMemberB.isEmpty()) {
                Set<MemberRole> roles = new HashSet<>();
                roles.add(MemberRole.ROLE_USER); // 일반 사용자 역할 부여

                Member userMember = Member.of(
                        "chillai@gmail.com",
                        passwordEncoder.encode("a"),
                        "chill~AI",
                        "chill~AI",
                        null, // 기본 프로필 이미지 사용
                        "local",
                        null,
                        roles
                );

                memberRepository.save(userMember);
                System.out.println("AI 멤버가 성공적으로 생성되었습니다.");
            } else {
                System.out.println("AI 멤버가 이미 존재합니다.");
            }

            // b@b 계정 확인 및 생성
            Optional<Member> existingMemberD = memberRepository.findByEmail("miniai@gmail.com");

            // 존재하지 않는 경우에만 생성
            if (existingMemberB.isEmpty()) {
                Set<MemberRole> roles = new HashSet<>();
                roles.add(MemberRole.ROLE_USER); // 일반 사용자 역할 부여

                Member userMember = Member.of(
                        "miniai@gmail.com",
                        passwordEncoder.encode("a"),
                        "미니 AI",
                        "미니 AI",
                        null, // 기본 프로필 이미지 사용
                        "local",
                        null,
                        roles
                );

                memberRepository.save(userMember);
                System.out.println("AI 멤버가 성공적으로 생성되었습니다.");
            } else {
                System.out.println("AI 멤버가 이미 존재합니다.");
            }

        };
    }
}