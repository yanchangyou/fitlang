package fit.spring.boot.servlet;

import cn.hutool.core.io.FileUtil;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.springframework.util.ResourceUtils;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;

@WebServlet(urlPatterns = "*.fit")
public class FitDispatcherServlet extends HttpServlet {

    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {

        String servletPath = req.getServletPath();
        if ("_api.fit".equals(servletPath)) {
            //TODO
        }
        File file = ResourceUtils.getFile("classpath:app" + servletPath);

        String fitCode = FileUtil.readUtf8String(file);
        String result = ExecuteJsonNodeUtil.executeCode(fitCode);

        resp.getWriter().write(result);

    }
}