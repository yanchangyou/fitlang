// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.jsonpath.inspections;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import fit.intellij.json.JsonBundle;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import fit.intellij.jsonpath.JsonPathConstants;
import fit.intellij.jsonpath.psi.JsonPathBinaryConditionalOperator;
import fit.intellij.jsonpath.psi.JsonPathTypes;
import fit.intellij.jsonpath.psi.JsonPathVisitor;
import org.jetbrains.annotations.NotNull;

import static fit.intellij.jsonpath.ui.JsonPathEvaluateManager.JSON_PATH_EVALUATE_EXPRESSION_KEY;

public final class JsonPathUnknownOperatorInspection extends LocalInspectionTool {
  @Override
  public @NotNull PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
    return new JsonPathVisitor() {
      @Override
      public void visitBinaryConditionalOperator(@NotNull JsonPathBinaryConditionalOperator operator) {
        super.visitBinaryConditionalOperator(operator);

        ASTNode namedOp = operator.getNode().findChildByType(JsonPathTypes.NAMED_OP);
        if (namedOp == null) return;

        String operatorName = namedOp.getText();

        if (!JsonPathConstants.STANDARD_NAMED_OPERATORS.contains(operatorName)) {
          boolean isEvaluateExpr = Boolean.TRUE.equals(holder.getFile().getUserData(JSON_PATH_EVALUATE_EXPRESSION_KEY));
          if (isEvaluateExpr) {
            holder.registerProblem(operator, JsonBundle.message("inspection.message.jsonpath.unsupported.jayway.operator", operatorName),
                                   ProblemHighlightType.ERROR);
          }
          else {
            holder.registerProblem(operator, null, JsonBundle.message("inspection.message.jsonpath.unknown.operator.name", operatorName));
          }
        }
      }
    };
  }
}
