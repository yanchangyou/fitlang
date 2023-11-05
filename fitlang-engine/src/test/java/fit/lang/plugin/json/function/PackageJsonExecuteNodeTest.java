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

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'package'," +
                "   'name': 'tool'," +
                "   'child': [" +
                "       {" +
                "           'name':'helloFail'," +
                "           'uni':'function'," +
                "           'child':{" +
                "               'uni':'hello'" +
                "           }," +
                "       }," +
                "   ]" +
                "}";

        try {
            ExecuteJsonNodeUtil.executeCode("{}", flow);
        } catch (Exception e) {
            Assert.assertEquals("package main function node is null, can not execute! main function name is: main.", e.getMessage());
        }
    }

    public void testExecute2() {
        String flow = "{" +
                "    'uni': 'package'," +
                "    'name': 'package1'," +
                "    'child': [" +
                "        {" +
                "            'uni': 'function'," +
                "            'name': 'hello1'," +
                "            'child': {" +
                "                'uni': 'hello'" +
                "            }" +
                "        }," +
                "        {" +
                "            'uni': 'function'," +
                "            'name': 'main'," +
                "            'child': {" +
                "                'uni': 'call'," +
                "                'function': 'package1.hello1'" +
                "            }" +
                "        }" +
                "    ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
        Assert.assertEquals("{\"message\":\"hello, world!\"}", result.toJSONString());
    }

    public void testExecute3() {
        String flow = "{" +
                "    'uni': 'package'," +
                "    'name': 'package1'," +
                "    'mainFunction': 'main2'," +
                "    'child': [" +
                "        {" +
                "            'uni': 'function'," +
                "            'name': 'hello2'," +
                "            'child': {" +
                "                'uni': 'hello'" +
                "            }" +
                "        }," +
                "        {" +
                "            'uni': 'function'," +
                "            'name': 'main2'," +
                "            'child': {" +
                "                'uni': 'call'," +
                "                'function': 'hello2'" +
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