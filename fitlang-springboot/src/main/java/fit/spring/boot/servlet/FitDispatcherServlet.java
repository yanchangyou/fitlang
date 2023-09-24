package fit.spring.boot.servlet;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StreamUtils;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.Charset;

@WebServlet(urlPatterns = "*.fit")
public class FitDispatcherServlet extends HttpServlet {

    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {

        String servletPath = req.getServletPath();
        if ("_api.fit".equals(servletPath)) {
            //TODO
        }
        File file = ResourceUtils.getFile("classpath:static" + servletPath);

        String fitCode = StreamUtils.copyToString(new FileInputStream(file), Charset.defaultCharset());

        String result = ExecuteJsonNodeUtil.executeCode(fitCode);

        resp.getWriter().write(result);

    }
}