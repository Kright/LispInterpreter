package com.github.kright

sealed class LispVmErrors(reason: String) extends Exception(reason)

class LispSyntaxError(reason: String) extends LispVmErrors(reason)
class LispNameError(reason: String) extends LispVmErrors(reason)
class LispRuntimeError(reason: String) extends LispVmErrors(reason)
class LispDebugError(reason: String) extends LispVmErrors(reason)