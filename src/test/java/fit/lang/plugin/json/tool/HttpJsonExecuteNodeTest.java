package fit.lang.plugin.json.tool;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HttpJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        HttpJsonExecuteNode httpJsonExecuteNode = new HttpJsonExecuteNode();
        String flow = "{" +//
                "   'url':'https://aip.baidubce.com/rest/2.0/ocr/v1/general_basic'," +
                "   'header':{" +
                "       'test':'123'" +
                "   }" +
                "}";
        String input = "{}";
        httpJsonExecuteNode.setNodeDefine(JSON.parseObject(flow));

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject(input), httpJsonExecuteNode);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());

        Assert.assertEquals("{\"error_code\":100,\"error_msg\":\"Invalid parameter\"}", output.toJSONString());

    }
}