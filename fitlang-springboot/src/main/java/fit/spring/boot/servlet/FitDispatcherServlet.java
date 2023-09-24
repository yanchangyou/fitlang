package fit.spring.boot.servlet;

import cn.hutool.core.io.IoUtil;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.springframework.util.ResourceUtils;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

@WebServlet(urlPatterns = "*.fit")
public class FitDispatcherServlet extends HttpServlet {

    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {

        String servletPath = req.getServletPath();
        if ("_api.fit".equals(servletPath)) {
            //TODO
        }

        InputStream inputStream = FitDispatcherServlet.class.getClassLoader().getResourceAsStream("static" + servletPath);

        String fitCode = IoUtil.readUtf8(inputStream);

        String result = ExecuteJsonNodeUtil.executeCode(fitCode);

        resp.getWriter().write(result);

    }
}