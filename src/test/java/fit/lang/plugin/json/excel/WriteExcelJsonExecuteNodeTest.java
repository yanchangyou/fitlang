package fit.lang.plugin.json.excel;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class WriteExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'writeExcel'," +
                "   'titleIndex': 1," +
                "   'titleConfig': {'a':{'title':'列名1'}}," +
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例-测试报告.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':[{'a':1},{'a':2}]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }
}