package org.scalafmt.config

import metaconfig._

sealed abstract class License(val text: String)

object License {
  case object `Apache-2.0` extends License(
    """Licensed under the Apache License, Version 2.0 (the "License");
      |you may not use this file except in compliance with the License.
      |You may obtain a copy of the License at
      |
      |    http://www.apache.org/licenses/LICENSE-2.0
      |
      |Unless required by applicable law or agreed to in writing, software
      |distributed under the License is distributed on an "AS IS" BASIS,
      |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      |See the License for the specific language governing permissions and
      |limitations under the License.""".stripMargin,
  )
  case object MIT extends License(
    """Permission is hereby granted, free of charge, to any person obtaining a copy
      |of this software and associated documentation files (the "Software"), to deal
      |in the Software without restriction, including without limitation the rights
      |to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      |copies of the Software, and to permit persons to whom the Software is
      |furnished to do so, subject to the following conditions:
      |
      |The above copyright notice and this permission notice shall be included in all
      |copies or substantial portions of the Software.
      |
      |THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      |IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      |FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      |AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      |LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      |OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      |SOFTWARE.""".stripMargin,
  )
  case object `BSD-2-Clause` extends License(
    """Redistribution and use in source and binary forms, with or without
      |modification, are permitted provided that the following conditions are met:
      |
      |1. Redistributions of source code must retain the above copyright notice, this
      |   list of conditions and the following disclaimer.
      |
      |2. Redistributions in binary form must reproduce the above copyright notice,
      |   this list of conditions and the following disclaimer in the documentation
      |   and/or other materials provided with the distribution.
      |
      |THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
      |AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
      |IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
      |DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
      |FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
      |DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
      |SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
      |CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
      |OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
      |OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.""".stripMargin,
  )
  case object `BSD-3-Clause` extends License(
    """Redistribution and use in source and binary forms, with or without
      |modification, are permitted provided that the following conditions are met:
      |
      |1. Redistributions of source code must retain the above copyright notice, this
      |   list of conditions and the following disclaimer.
      |
      |2. Redistributions in binary form must reproduce the above copyright notice,
      |   this list of conditions and the following disclaimer in the documentation
      |   and/or other materials provided with the distribution.
      |
      |3. Neither the name of the copyright holder nor the names of its
      |   contributors may be used to endorse or promote products derived from
      |   this software without specific prior written permission.
      |
      |THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
      |AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
      |IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
      |DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
      |FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
      |DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
      |SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
      |CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
      |OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
      |OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.""".stripMargin,
  )
  case object `MPL-2.0` extends License(
    """This Source Code Form is subject to the terms of the Mozilla Public
      |License, v. 2.0. If a copy of the MPL was not distributed with this
      |file, You can obtain one at https://mozilla.org/MPL/2.0/.""".stripMargin,
  )
  case object `GPL-2.0-only` extends License(
    """This program is free software; you can redistribute it and/or modify
      |it under the terms of the GNU General Public License as published by
      |the Free Software Foundation; version 2.
      |
      |This program is distributed in the hope that it will be useful,
      |but WITHOUT ANY WARRANTY; without even the implied warranty of
      |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
      |GNU General Public License for more details.
      |
      |You should have received a copy of the GNU General Public License along
      |with this program; if not, write to the Free Software Foundation, Inc.,
      |51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.""".stripMargin,
  )
  case object `GPL-3.0-only` extends License(
    """This program is free software: you can redistribute it and/or modify
      |it under the terms of the GNU General Public License as published by
      |the Free Software Foundation, version 3.
      |
      |This program is distributed in the hope that it will be useful,
      |but WITHOUT ANY WARRANTY; without even the implied warranty of
      |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
      |GNU General Public License for more details.
      |
      |You should have received a copy of the GNU General Public License
      |along with this program. If not, see <https://www.gnu.org/licenses/>.""".stripMargin,
  )
  case object `LGPL-2.1-only` extends License(
    """This library is free software; you can redistribute it and/or modify
      |it under the terms of the GNU Lesser General Public License as
      |published by the Free Software Foundation; version 2.1.
      |
      |This library is distributed in the hope that it will be useful,
      |but WITHOUT ANY WARRANTY; without even the implied warranty of
      |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
      |GNU Lesser General Public License for more details.
      |
      |You should have received a copy of the GNU Lesser General Public
      |License along with this library; if not, write to the Free Software
      |Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.""".stripMargin,
  )
  case object `LGPL-3.0-only` extends License(
    """This library is free software: you can redistribute it and/or modify
      |it under the terms of the GNU Lesser General Public License as
      |published by the Free Software Foundation, version 3.
      |
      |This library is distributed in the hope that it will be useful,
      |but WITHOUT ANY WARRANTY; without even the implied warranty of
      |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
      |GNU Lesser General Public License for more details.
      |
      |You should have received a copy of the GNU Lesser General Public
      |License along with this library. If not, see
      |<https://www.gnu.org/licenses/>.""".stripMargin,
  )
  case object `EPL-2.0` extends License(
    """This program and the accompanying materials are made available under the
      |terms of the Eclipse Public License v. 2.0 which is available at
      |https://www.eclipse.org/legal/epl-2.0.
      |
      |This Source Code may also be made available under the following Secondary
      |Licenses when the conditions for such availability set forth in the Eclipse
      |Public License v. 2.0 are satisfied: GNU General Public License, version 2
      |with the GNU Classpath Exception which is available at
      |https://www.gnu.org/software/classpath/license.html.
      |
      |SPDX-License-Identifier: EPL-2.0 OR GPL-2.0 WITH Classpath-exception-2.0""".stripMargin,
  )
  case object ISC extends License(
    """Permission to use, copy, modify, and/or distribute this software for any
      |purpose with or without fee is hereby granted, provided that the above
      |copyright notice and this permission notice appear in all copies.
      |
      |THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
      |WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
      |MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
      |ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
      |WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
      |ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
      |OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.""".stripMargin,
  )
  case object Unlicense extends License(
    """This is free and unencumbered software released into the public domain.
      |
      |Anyone is free to copy, modify, publish, use, compile, sell, or
      |distribute this software, either in source code form or as a compiled
      |binary, for any purpose, commercial or non-commercial, and by any means.
      |
      |In jurisdictions that recognize copyright laws, the author or authors of
      |this software dedicate any and all copyright interest in the software to
      |the public domain. We make this dedication for the benefit of the public
      |at large and to the detriment of our heirs and successors. We intend this
      |dedication to be an overt act of relinquishment in perpetuity of all
      |present and future rights to this software under copyright law.
      |
      |THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
      |OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
      |MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
      |IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      |LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
      |FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
      |DEALINGS IN THE SOFTWARE.
      |
      |For more information, please refer to <https://unlicense.org>""".stripMargin,
  )
  implicit val codec: ConfCodecEx[License] = ConfCodecEx.oneOf[License](
    `Apache-2.0`, MIT, `BSD-2-Clause`, `BSD-3-Clause`,
    `MPL-2.0`, `GPL-2.0-only`, `GPL-3.0-only`,
    `LGPL-2.1-only`, `LGPL-3.0-only`,
    `EPL-2.0`, ISC, Unlicense,
  )

  def detailed(license: License, copyrightLine: Option[String]): String = {
    val header = copyrightLine.fold("")(_ + "\n\n")
    header + license.text
  }
}
