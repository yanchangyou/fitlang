package fit.lang;

import cn.hutool.core.util.NumberUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Test {

    public static void main(String[] args) {
        String text = "abc123def456.23e2";
        List<Number> list = parseNumbers(text);
        System.out.println(list);
        System.out.println(NumberUtil.max(list.toArray(new Long[0])));
    }

    public static List<Number> parseNumbers(String text) {
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(text);
        List<Number> list = new ArrayList<>();
        while (matcher.find()) {
            String one = matcher.group();
            list.add(Long.parseLong(one));
        }
        return list;
    }
}
