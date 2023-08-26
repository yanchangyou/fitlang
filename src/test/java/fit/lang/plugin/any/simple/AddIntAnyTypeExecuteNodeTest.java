package fit.lang.plugin.any.simple;

import fit.lang.plugin.any.define.AnyTypeExecuteContext;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;
import fit.lang.plugin.any.math.AddIntAnyTypeExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.ArrayList;
import java.util.List;

public class AddIntAnyTypeExecuteNodeTest extends TestCase {

    public void testExecute() {

        AddIntAnyTypeExecuteNode executeNode = new AddIntAnyTypeExecuteNode();

        AnyTypeExecuteContext nodeContext = new AnyTypeExecuteContext();

        AnyTypeExecuteNodeInput input = new AnyTypeExecuteNodeInput(nodeContext);
        AnyTypeExecuteNodeOutput output = new AnyTypeExecuteNodeOutput(nodeContext);

        List<Integer> integerList = new ArrayList<>();

        integerList.add(1);
        integerList.add(2);
        integerList.add(3);

        input.setNodeData(new AnyTypeExecuteNodeData<>(integerList));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), 6);


    }
}