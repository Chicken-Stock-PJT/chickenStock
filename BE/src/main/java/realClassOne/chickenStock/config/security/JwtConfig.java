package realClassOne.chickenStock.config.security;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

@Configuration
@Getter
public class JwtConfig {

    @Value("${app.jwt.secret}")
    private String jwtSecret;

    @Value("${app.jwt.expiration.access}")
    private long jwtAccessExpirationMs;

    @Value("${app.jwt.expiration.refresh}")
    private long jwtRefreshExpirationMs;

}
