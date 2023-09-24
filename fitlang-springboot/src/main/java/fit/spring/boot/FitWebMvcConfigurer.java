package fit.spring.boot;

import fit.spring.boot.servlet.FitDispatcherServlet;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configurable
public class FitWebMvcConfigurer implements WebMvcConfigurer {

    @Bean
    public ServletRegistrationBean registerFitServlet() {
        return new ServletRegistrationBean(new FitDispatcherServlet());
    }

}
