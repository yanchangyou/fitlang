package fit.lang.plugin.json.excel;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'headerIndex': 1," +
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("sheetData"));

    }


    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'headerIndex': 0," +
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
                "}";
        try {
            ExecuteJsonNodeUtil.executeCode("{}", flow);
        } catch (Exception e) {
            Assert.assertTrue(e.getMessage().contains("excel headerIndex must be great than 0, but found: 0"));
        }
    }
}