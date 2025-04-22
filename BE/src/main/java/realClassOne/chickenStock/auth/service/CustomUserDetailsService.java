package realClassOne.chickenStock.auth.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.member.entity.Member;
import realClassOne.chickenStock.member.exception.MemberErrorCode;
import realClassOne.chickenStock.member.repository.MemberRepository;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {
    private final MemberRepository memberRepository;

    @Override
    @Transactional(readOnly = true)
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {

        System.out.println(email);
        boolean exists = memberRepository.existsByEmail(email);
        log.info("이메일 회원 존재 여부: {}", exists);

        Member member = memberRepository.findByEmail(email).orElseThrow(() -> new CustomException(MemberErrorCode.MEMBER_NOT_FOUND));

        List<GrantedAuthority> authorities = member.getRoles().stream().map(role -> new SimpleGrantedAuthority(role.name())).collect(Collectors.toList());

        return new org.springframework.security.core.userdetails.User(member.getEmail(), member.getPassword(), authorities);
    }

    public MemberRepository getMemberRepository() {
        return memberRepository;
    }
}
