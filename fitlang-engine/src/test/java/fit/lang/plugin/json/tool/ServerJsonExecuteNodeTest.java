package fit.lang.plugin.json.tool;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class ServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'server'," +
                "   'port': 11110," +
                "   'root': '/opt/log'," +
                "   'action':{" +
                "       '/hello':{" +
                "           'uni':'hello'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }


    public void testExecute1() throws InterruptedException {
        String flow = "{\n" +
                "    \"uni\": \"loop\",\n" +
                "    \"loopTimes\": 2,\n" +
                "    \"isPipe\": true,\n" +
                "    \"isBagsMode\": true,\n" +
                "    \"bagsName\": 'list',\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"mix\",\n" +
                "            \"json\": {\n" +
                "                \"port\": \"${10111+loopIndex}\"\n" +
                "            }\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"server\",\n" +
                "            \"welcome\": \"welcome to fit lang world!\",\n" +
                "            \"action\": {\n" +
                "                \"/hello\": {\n" +
                "                    \"uni\": \"hello\"\n" +
                "                }\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }

}