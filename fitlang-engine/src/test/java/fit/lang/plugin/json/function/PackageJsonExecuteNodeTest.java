package fit.lang.plugin.json.function;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class PackageJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'package'," +
                "   'name': 'tool'," +
                "   'mainFunction': 'hello'," +
                "   'child': [" +
                "       {" +
                "           'name':'hello'," +
                "           'uni':'function'," +
                "           'child':{" +
                "               'uni':'hello'" +
                "           }," +
                "       }," +
                "   ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
        Assert.assertEquals("{\"message\":\"hello, world!\"}", result.toJSONString());
    }
}