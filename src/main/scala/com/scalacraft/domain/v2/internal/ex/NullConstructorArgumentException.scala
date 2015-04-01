/*
   Copyright 2014 - 2015 Janek Bogucki

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package com.scalacraft.domain.v2.internal.ex

/**
 * Exception to throw when a constructor argument is null.
 *
 * This is not part of the public API because the public API does not support
 * null constructor arguments.
 */
case class NullConstructorArgumentException(paramName: String)
  extends IllegalArgumentException(paramName)