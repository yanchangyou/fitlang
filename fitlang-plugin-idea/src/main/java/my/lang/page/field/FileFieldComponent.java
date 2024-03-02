package my.lang.page.field;

import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.JBEmptyBorder;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class FileFieldComponent extends JPanel {

    JTextField field;

    String value;

    public FileFieldComponent(String value, Project project) {
        this.value = value;

        setLayout(new BorderLayout());

        field = new JTextField(value);
        field.setBorder(new JBEmptyBorder(0));
        add(field, BorderLayout.CENTER);

        JButton button = new JButton("...");
        add(button, BorderLayout.EAST);

        button.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                boolean chooseFiles = true;
                boolean chooseFolders = false;
                boolean chooseJars = false;
                boolean chooseJarsAsFiles = false;
                boolean chooseJarContents = false;
                boolean chooseMultiple = false;
                FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(chooseFiles,
                        chooseFolders,
                        chooseJars,
                        chooseJarsAsFiles,
                        chooseJarContents,
                        chooseMultiple);
                VirtualFile file = FileChooser.chooseFile(fileChooserDescriptor, project, null);
                if (file != null) {
                    field.setText(file.getPath());
                }
            }
        });
    }

    public String getText() {
        return field.getText();
    }


}
