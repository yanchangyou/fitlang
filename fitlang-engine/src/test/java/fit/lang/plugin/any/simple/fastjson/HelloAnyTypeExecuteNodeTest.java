package fit.lang.plugin.any.simple.fastjson;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteContext;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;
import fit.lang.plugin.any.simple.HelloAnyTypeExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloAnyTypeExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new HelloAnyTypeExecuteNode();

        AnyTypeExecuteContext nodeContext = new AnyTypeExecuteContext();

        AnyTypeExecuteNodeInput input = new AnyTypeExecuteNodeInput(nodeContext);
        AnyTypeExecuteNodeOutput output = new AnyTypeExecuteNodeOutput(nodeContext);

        JSONObject data =new JSONObject();
        data.put("who", "world");
        input.setNodeData(new AnyTypeExecuteNodeData<>(data));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), "hello, {\"who\":\"world\"}!");


    }
}