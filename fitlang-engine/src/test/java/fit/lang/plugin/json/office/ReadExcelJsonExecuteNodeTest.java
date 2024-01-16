package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'file': '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例.xls'" +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertNotNull(outputJson.get("name"));

    }

    public void testExecuteWithHeader() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'file': '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例.xls'," +
                "   'header': {'code':'接口编码','name':'接口名称', 'description':'接口描述','url':'URL'}," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertNotNull(outputJson.get("name"));

    }
}