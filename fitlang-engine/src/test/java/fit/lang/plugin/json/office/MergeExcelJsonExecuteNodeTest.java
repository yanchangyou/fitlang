package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class MergeExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'mergeExcel'," +
                "   'sheetName': '接口列表'," +
                "   'inputFiles': [" +
                "               '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例.xls'," +
                "               '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例.xls'" +
                "    ]," +
                "   'outputFile': '/opt/github/fitlang/fitlang-engine/src/test/resources/fit/lang/plugin/json/excel/测试用例-merge.xls'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertNotNull(outputJson.get("rows"));

    }
}