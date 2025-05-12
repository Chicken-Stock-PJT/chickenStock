package realClassOne.chickenStock;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.retry.annotation.EnableRetry;

@EnableJpaAuditing
@SpringBootApplication
@EnableRetry
public class ChickenStockApplication {

	public static void main(String[] args) {
		SpringApplication.run(ChickenStockApplication.class, args);
	}

}
