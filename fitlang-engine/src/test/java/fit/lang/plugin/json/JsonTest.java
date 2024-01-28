package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import junit.framework.TestCase;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseJsonSchema;

public class JsonTest extends TestCase {

    public void test() {
//        System.out.println(JSONObject.parse("{1,2}"));
//        System.out.println(JSONObject.parse("{001,2}"));
//        System.out.println(JSONObject.parse("{1,02}"));
//        System.out.println(JSONObject.parse("{01,02}"));
//        System.out.println(JSONObject.parse("{10,2}"));
//        System.out.println(JSONObject.parse("{1,20}"));
//        System.out.println(JSONObject.parse("{100,2}"));
//        System.out.println(JSONObject.parse("{1,200}"));
    }

    public void testFormat() {
        System.out.println(JSONObject.parseObject("{'foo':'bar'}").toJSONString(JSONWriter.Feature.PrettyFormat));
    }

    public void testSchema() {
        JSONObject jsonObject = JSONObject.parseObject("{'foo':'bar'}");
        JSONObject schema = parseJsonSchema(jsonObject);
        System.out.println(schema);
    }

    public void testSchemaObject() {
        JSONObject jsonObject = JSONObject.parseObject("{'object':{'foo':'bar'}}");
        JSONObject schema = parseJsonSchema(jsonObject);
        System.out.println(schema);
    }

}