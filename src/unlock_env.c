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

/* This is derived from envir.c in the R 2.15.1 source from
  code found in:
  https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
*/

#include <R.h>
#include <Rinternals.h>

#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

SEXP unlock_envir(SEXP env)
{
  if (TYPEOF(env) == NILSXP)
    error("use of NULL environment is defunct");
  if (TYPEOF(env) != ENVSXP)
    error("not an environment");
  
  UNLOCK_FRAME(env);
  
  // Return TRUE if unlocked; FALSE otherwise
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, 1));
  LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
  UNPROTECT(1);
  
  return result;
}
