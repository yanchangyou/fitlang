// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.navigation;

import fit.intellij.json.JsonBundle;
import kotlin.NotImplementedError;

public enum JsonQualifiedNameKind {
  Qualified,
  JsonPointer;

  @Override
  public String toString() {
    switch (this) {
      case Qualified:
        return fit.intellij.json.JsonBundle.message("qualified.name.qualified");
      case JsonPointer:
        return JsonBundle.message("qualified.name.pointer");
    }
    throw new NotImplementedError("Unknown name kind: " + this.name());
  }
}
