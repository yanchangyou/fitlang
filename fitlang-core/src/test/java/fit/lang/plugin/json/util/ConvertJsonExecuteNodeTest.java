package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertJsonExecuteNodeTest extends TestCase {

    public void test() {

        ConvertJsonExecuteNode convertNode = new ConvertJsonExecuteNode();

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = buildInput(nodeContext);
        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        JSONObject[] expresses = buildExpressList();
        JSONObject valueMapping = buildValueMapping();
        String[] expects = buildExpected();
        int index = 0;
        for (JSONObject express : expresses) {
            convertNode.setExpress(express);
            convertNode.setValueMapping(valueMapping);
            convertNode.execute(input, output);
            System.out.println(output.getData());
            Assert.assertEquals(expects[index], output.getData().toJSONString());
            output.getData().clear();
            index++;
        }
    }

    String[] buildExpected() {
        String[] expects = new String[]{
                "{\"a\":1,\"b\":\"B\",\"c\":[1,2],\"d\":[{\"d1\":1,\"d2\":\"d2\",\"d3\":{\"d30\":0}}]}",
                "{\"A\":1,\"B\":\"1\",\"C\":[1,2],\"D\":{\"a\":1,\"b\":\"1\"}}",
                "{\"A\":{\"A1\":\"甲\"},\"B\":{\"B1\":\"1\"},\"C\":{\"C1\":[1,2]}}",
                "{\"A\":{\"A1\":\"甲\"},\"B\":{\"B1\":\"1\"},\"C\":{\"C1\":[1,2]},\"D\":{\"D1\":{\"a\":1,\"b\":\"1\"}}}",
                "{\"A\":[{\"A1\":\"甲\"}],\"B\":{\"B1\":\"1\"},\"C\":{\"C1\":[1,2]},\"D\":{\"D1\":{\"a\":1,\"b\":\"1\"}}}",
                "{\"A\":{\"A1\":\"甲\"},\"B\":{\"B1\":\"1\"},\"C\":{\"C1\":[1,2]},\"D\":{\"D1\":{\"a\":1,\"b\":\"1\"}},\"E\":[{\"E1\":\"甲\",\"E2\":\"1\"},{\"E1\":\"乙\",\"E2\":\"2\"}]}",
                "{\"A\":{\"A1\":\"甲\"},\"B\":{\"B1\":\"1\"},\"C\":{\"C1\":[1,2]},\"D\":{\"D1\":{\"a\":1,\"b\":\"1\"}},\"E\":[{\"E1\":\"甲\"},{\"E1\":\"乙\"}]}",
                "{\"E\":[{\"E3\":[{\"E1\":\"甲\",\"E2\":\"1\"},{\"E1\":11,\"E2\":\"11\"}]},{\"E3\":[{\"E1\":\"乙\",\"E2\":\"2\"},{\"E1\":\"乙乙\",\"E2\":\"22\"}]}]}",
        };
        return expects;
    }

    JSONObject[] buildExpressList() {
        String[] expresses = new String[]{
                "{'a':1,'b':'B','c':[1,2],'d':[{'d1':1,'d2':'d2','d3':{'d30':0}}]}",
                "{'A':'${a}','B':'${b}','C':'${c}','D':'${d}'}",
                "{'A.A1':'${a}','B.B1':'${b}','C.C1':'${c}'}",
                "{'A.A1':'${a}','B.B1':'${b}','C.C1':'${c}','D.D1':'${d}'}",
                "{'A[0].A1':'${a}','B.B1':'${b}','C.C1':'${c}','D.D1':'${d}'}",
                "{'A.A1':'${a}','B.B1':'${b}','C.C1':'${c}','D.D1':'${d}','E[].E1':'${e[].a}','E[].E2':'${e[].b}'}",
                "{'A.A1':'${a}','B.B1':'${b}','C.C1':'${c}','D.D1':'${d}','E[].E1':'${e[].a}'}",
                "{'E[].E3[].E1':'${f[].c[].c1}','E[].E3[].E2':'${f[].c[].c2}'}",
        };
        JSONObject[] list = new JSONObject[expresses.length];
        int index = 0;
        for (String express : expresses) {
            list[index++] = JSON.parseObject(express);
        }
        return list;
    }

    JsonExecuteNodeInput buildInput(JsonExecuteContext nodeContext) {

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        input.set("a", 1);
        input.set("b", "1");
        input.set("c", JSON.parseArray("[1,2]"));
        input.set("d", JSON.parseObject("{'a':1,'b':'1'}"));
        input.set("e", JSON.parseArray("[{'a':1,'b':'1'},{'a':2,'b':'2'}]"));
        input.set("f", JSON.parseArray("[{'a':1,'b':'1','c':[{'c1':1,'c2':'1'},{'c1':11,'c2':'11'}]},{'a':2,'b':'2','c':[{'c1':2,'c2':'2'},{'c1':22,'c2':'22'}]}]"));

        return input;
    }

    JSONObject buildValueMapping() {
        return JSON.parseObject("{'A1':{'1':'甲'},'E1':{'1':'甲','22':'乙乙','2':'乙'}}");
    }

}