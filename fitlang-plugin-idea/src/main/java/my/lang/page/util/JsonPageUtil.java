package my.lang.page.util;

import javax.swing.*;

/**
 * 工具类
 */
public class JsonPageUtil {

    public static void adjustSplitPanel(JSplitPane splitPane, double splitRatio) {
        new Thread(() -> {
            for (int i = 0; i < 4; i++) {
                try {
                    Thread.sleep(500L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                splitPane.setDividerLocation(splitRatio);
            }
        }).start();
    }
}
