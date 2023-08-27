package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;

public class ExpressUtil {

    private static final AviatorEvaluatorInstance engine = AviatorEvaluator.getInstance();

    public static Object eval(String express, JSONObject param) {
        if (express == null) {
            return null;
        }

        express = express.trim();
        if (!express.startsWith("${") || !express.endsWith("}")) {
            return express;
        }

        String realExpress = express.substring(2, express.length() - 1);

        return engine.execute(realExpress, param);
    }
}
