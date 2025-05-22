package realClassOne.chickenStock;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.EnableAsync;

@EnableJpaAuditing
@SpringBootApplication
@EnableRetry
@EnableAsync(proxyTargetClass = true)  // CGLIB 프록시 사용
public class ChickenStockApplication {

	public static void main(String[] args) {
		SpringApplication.run(ChickenStockApplication.class, args);
	}

}
