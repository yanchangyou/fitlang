package fit.spring.boot;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringBootVersion;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@ServletComponentScan
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

    @RequestMapping("/execute")
    @ResponseBody
    public Object execute(@RequestBody String nodeDefine) {
        return ExecuteJsonNodeUtil.executeCode(nodeDefine);
    }

}