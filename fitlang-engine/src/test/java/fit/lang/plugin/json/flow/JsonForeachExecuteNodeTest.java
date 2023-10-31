package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import fit.lang.common.util.PrintExecuteNode;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonForeachExecuteNodeTest extends TestCase {

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'foreach'," +
                "   'foreachField': 'list'," +
                "   'mixToItemField': 'times'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':['a'],'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"value\":\"a\",\"times\":0,\"message\":\"mix\"}]}", output);
    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'foreach'," +
                "   'foreachField': 'list'," +
                "   'mixToItemField': 'times'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':{'code':'001'},'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"key\":\"code\",\"value\":\"001\",\"times\":0,\"message\":\"mix\"}]}", output);
    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'foreach'," +
                "   'foreachField': 'list'," +
                "   'mixToItemField': 'times'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':{'object':{'foo':'bar'}},'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"key\":\"object\",\"value\":{\"foo\":\"bar\"},\"times\":0,\"message\":\"mix\"}]}", output);
    }

    public void testExecuteIsPipe() {
        String flow = "{" +//
                "   'uni': 'foreach'," +
                "   'foreachField': 'list'," +
                "   'isPipe': true," +
                "   'child': [" +
                "   {" +
                "       'uni':'increase'," +
                "   }," +
                "   {" +
                "       'uni':'increase'," +
                "   }" +
                "]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':[1,2],'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"value\":1,\"index\":2},{\"value\":2,\"index\":2}]}", output);
    }

    public void testExecuteIsNotPipe() {
        String flow = "{" +//
                "   'uni': 'foreach'," +
                "   'foreachField': 'list'," +
                "   'isPipe': false," +
                "   'child': [" +
                "   {" +
                "       'uni':'increase'," +
                "   }," +
                "   {" +
                "       'uni':'increase'," +
                "   }" +
                "]" +
                "}";
        System.out.println(flow);

        String output = ExecuteJsonNodeUtil.executeCode("{'list':[1,2],'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"value\":1,\"index\":1},{\"value\":2,\"index\":1}]}", output);
    }
}