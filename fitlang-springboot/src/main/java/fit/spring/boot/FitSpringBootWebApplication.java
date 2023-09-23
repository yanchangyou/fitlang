package fit.spring.boot;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringBootVersion;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
public class FitSpringBootWebApplication {

    public static void main(String[] args) {

        if (SpringBootVersion.getVersion().startsWith("1.")) {
            // spring boot 1.5.21.RELEASE在linux下面报错，需要去掉参数
            SpringApplication.run(FitSpringBootWebApplication.class);
        } else {
            //spring boot 2 不能去掉参
            SpringApplication.run(FitSpringBootWebApplication.class, args);
        }
    }

    @RequestMapping("/hello")
    public Object hello() {
        return "hello, fit!";
    }

}