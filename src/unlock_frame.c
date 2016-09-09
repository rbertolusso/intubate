// Copyright (C) 2016 Roberto Bertolusso
//
// This file is part of intubate.
//
// intubate is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// intubate is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with intubate. If not, see <http://www.gnu.org/licenses/>.

/* The source of inspiration is a gist by Winston Chang:
 https://gist.github.com/wch/3280369 on the file called
 unlockEnvironment.r (Thanks Winston!)
*/

#include <R.h>
#include <Rinternals.h>

#define FRAME_LOCK_MASK (1<<14)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

void unlock_frame(SEXP env)
{
  UNLOCK_FRAME(env);
}
