package fit.lang.plugin.json.http;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HttpJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +
                "    'uni': 'postman'," +
                "    'method': 'POST'," +
                "    'url': 'http://fit.321zou.com/hello'," +
                "    'header': {" +
                "        'contentType': 'application/json'" +
                "    }," +
                "    'query': {" +
                "        'foo': 'bar'" +
                "    }," +
                "    'body': {" +
                "        'hello': 'world'" +
                "    }," +
                "    'proxy': {" +
                "        'host': ''," +
                "        'port': 0" +
                "    }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"message\":\"hello, world!\"}", output);
    }

}