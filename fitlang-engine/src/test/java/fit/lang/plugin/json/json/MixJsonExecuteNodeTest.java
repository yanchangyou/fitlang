package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class MixJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'json':{" +
                "       'hello':'world'" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }

    public void testTestExecute1() {

        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'json':{" +
                "       'hello':\"${who}\"" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }

    public void testExecute2() {

        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'pickJsonField': 'hello'," +
                "   'json':{" +
                "       'hello':{'hello':'${who}'}" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }

    public void testExecute3() {

        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'pickJsonField': 'hello'," +
                "   'json':{" +
                "       'hello':{'hello':'world'}" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }

    public void testExecute4() {

        String flow = "{\n" +
                "    \"uni\": \"loop\",\n" +
                "    \"loopTimes\": 10,\n" +
                "    \"isPipe\": true,\n" +
                "    \"child\": {\n" +
                "        \"uni\": \"mix\",\n" +
                "        \"json\": {\n" +
                "            \"array\": \"${array+=[loopIndex]}\"\n" +
                "        }\n" +
                "    }\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode(" {\n" +
                "        \"array\": [\n" +
                "            1\n" +
                "        ]\n" +
                "    }", flow);

        System.out.println(output);
        Assert.assertEquals("{\"array\":[1,0,1,2,3,4,5,6,7,8,9]}", output);
    }
}