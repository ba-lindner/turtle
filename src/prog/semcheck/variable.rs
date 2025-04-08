use crate::{tokens::{ValType, Variable, VariableKind}, TurtleError};

use super::{CheckContext, TypeError, Vars};

impl Variable {
    pub(super) fn val_type(&mut self, ctx: &mut CheckContext) -> Result<(ValType, Vars), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if let Some(l) = ctx.locals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *l;
                    } else {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
                Ok((*vt, Vars(*vt != ValType::Any, true)))
            }
            VariableKind::Global(idx, vt) => {
                if let Some(g) = ctx.globals.get(idx) {
                    if *vt == ValType::Any {
                        *vt = *g;
                    } else {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
                Ok((*vt, Vars(true, *vt != ValType::Any)))
            }
            VariableKind::GlobalPreDef(pdv) => Ok((pdv.val_type(&ctx.features), Vars::new())),
        }
    }

    pub(super) fn expect_type(&mut self, ty: ValType, ctx: &mut CheckContext) -> Result<(), TurtleError> {
        let e_map = |e: TypeError| TurtleError::TypeError(e, self.pos);
        match &mut self.kind {
            VariableKind::Local(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(l) = ctx.locals.get(idx) {
                        l.assert(ty).map_err(e_map)?;
                        *vt = *l;
                    } else {
                        ctx.locals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(l) = ctx.locals.get(idx) {
                        vt.assert(*l).map_err(e_map)?;
                    }
                }
            }
            VariableKind::Global(idx, vt) => {
                if *vt == ValType::Any {
                    if let Some(g) = ctx.globals.get(idx) {
                        g.assert(ty).map_err(e_map)?;
                        *vt = *g;
                    } else {
                        ctx.globals.insert(*idx, ty);
                        *vt = ty;
                    }
                } else {
                    vt.assert(ty).map_err(e_map)?;
                    if let Some(g) = ctx.globals.get(idx) {
                        vt.assert(*g).map_err(e_map)?;
                    }
                }
            }
            VariableKind::GlobalPreDef(pdv) => {
                pdv.val_type(&ctx.features).assert(ty).map_err(e_map)?
            }
        }
        Ok(())
    }
}