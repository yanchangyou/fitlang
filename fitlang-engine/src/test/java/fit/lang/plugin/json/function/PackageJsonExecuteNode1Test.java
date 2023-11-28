package fit.lang.plugin.json.function;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class PackageJsonExecuteNode1Test extends TestCase {

    public void testExecute3() {
        JsonPackageExecuteNode.addImportPath("/opt/github/fitlang/fitlang-server/demo/fitserver/app/package/");

        String flow = "{" +
                "    'uni': 'package'," +
                "    'name': 'package3'," +
                "    'import': [" +
                "        'package2'," +
                "        'package/package2'" +
                "    ]," +
                "    'child': [" +
                "        {" +
                "            'uni': 'function'," +
                "            'name': 'main'," +
                "            'child': {" +
                "                'uni': 'call'," +
                "                'function': 'package0.hello'" +
                "            }" +
                "        }" +
                "    ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
        Assert.assertEquals("{\"message\":\"hello, world!\"}", result.toJSONString());
    }
}