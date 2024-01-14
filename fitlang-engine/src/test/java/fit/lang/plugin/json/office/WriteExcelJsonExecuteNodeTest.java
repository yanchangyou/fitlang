package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class WriteExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'writeExcel'," +
                "   'file': '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例-2.xls'," +
                "   'arrayField':'list'," +
                "   'isAppend': true," +
                "}";

        String input = "{'list':[{'column0':'c0','column1':'c1','column2':'c2','column3':'c3'}]}";
        String output = ExecuteJsonNodeUtil.executeCode(input, flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertNotNull(outputJson.get("result"));

    }
}