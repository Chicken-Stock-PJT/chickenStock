package realClassOne.chickenStock.stock.trade.service;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Bean
    public FilterRegistrationBean<RequestRateLimitFilter> rateLimitFilter() {
        FilterRegistrationBean<RequestRateLimitFilter> registrationBean = new FilterRegistrationBean<>();
        registrationBean.setFilter(new RequestRateLimitFilter());
        registrationBean.addUrlPatterns("/api/stock/trading/*");
        registrationBean.setOrder(1);
        return registrationBean;
    }
}