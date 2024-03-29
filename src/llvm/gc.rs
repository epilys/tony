pub struct HeapArray;

use super::*;

impl HeapArray {
    pub fn new<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        type_span: &Span<TonyType>,
        len: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        // TODO: compare with zero, must be positive
        let result_type =
            Compiler::tonytype_into_basictypenum(compiler.context, type_span.into_inner());
        let type_len = result_type.size_of().unwrap();
        let new_array_fn = compiler.get_function("alloc__").unwrap();

        let ptr_value = compiler
            .builder
            .build_call(new_array_fn, &[len.into(), type_len.into()], "new_array")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();
        compiler
            .builder
            .build_pointer_cast(
                ptr_value,
                result_type.ptr_type(AddressSpace::Generic),
                format!("ptr_cast_{:?}", type_span).as_str(),
            )
            .into()
    }

    pub fn index<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        id_span: &Span<Identifier>,
        idx: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, TonyError> {
        let name = &id_span.into_inner().0.to_string();
        let ptr = compiler.variables.get(name.as_str()).ok_or_else(|| {
            TonyError::with_span(
                format!("Undefined variable, found {:?}", id_span.into_inner()),
                id_span.span(),
            )
            .set_symbol_table_kind()
        })?;
        let zero = compiler.context.i32_type().const_zero();
        let ptr_val = compiler
            .builder
            .build_load(
                unsafe { compiler.builder.build_gep(ptr.ptr, &[zero], "tmpptr") },
                "tmpptr",
            )
            .into_pointer_value();
        Ok(unsafe { compiler.builder.build_gep(ptr_val, &[idx], "tmpidx") })
    }

    pub fn incref<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), TonyError> {
        let incref_fn = compiler.get_function("incref__").unwrap();

        let ptr_val = compiler
            .builder
            .build_pointer_cast(
                compiler
                    .builder
                    .build_load(ptr, "tmpptr")
                    .into_pointer_value(),
                compiler.context.i64_type().ptr_type(AddressSpace::Generic),
                "incref_ptr_cast",
            )
            .into();
        compiler
            .builder
            .build_call(incref_fn, &[ptr_val], "incref__");
        Ok(())
    }

    pub fn decref<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), TonyError> {
        let decref_fn = compiler.get_function("decref__").unwrap();
        let ptr_val = compiler.builder.build_pointer_cast(
            compiler
                .builder
                .build_load(ptr, "tmpptr")
                .into_pointer_value(),
            compiler.context.i64_type().ptr_type(AddressSpace::Generic),
            "decref_ptr_cast",
        );
        compiler
            .builder
            .build_call(decref_fn, &[ptr_val.into()], "decref__");
        Ok(())
    }

    pub fn incref2<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), TonyError> {
        let incref_fn = compiler.get_function("incref__").unwrap();

        let ptr_val = compiler
            .builder
            .build_pointer_cast(
                ptr,
                compiler.context.i64_type().ptr_type(AddressSpace::Generic),
                "incref_ptr_cast",
            )
            .into();
        compiler
            .builder
            .build_call(incref_fn, &[ptr_val], "incref__");
        Ok(())
    }

    pub fn decref2<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<(), TonyError> {
        let decref_fn = compiler.get_function("decref__").unwrap();
        let ptr_val = compiler
            .builder
            .build_pointer_cast(
                ptr,
                compiler.context.i64_type().ptr_type(AddressSpace::Generic),
                "decref_ptr_cast",
            )
            .into();
        compiler
            .builder
            .build_call(decref_fn, &[ptr_val], "decref__");
        Ok(())
    }
}

pub struct HeapList;

use super::*;

impl HeapList {
    pub fn cons<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        head: PointerValue<'ctx>,
        tail: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let cons_fn = compiler.get_function("list_cons__").unwrap();
        let element_type_len = head.get_type().get_element_type().size_of().unwrap().into();

        let head_ptr = compiler.builder.build_pointer_cast(
            head,
            compiler.context.i64_type().ptr_type(AddressSpace::Generic),
            "cons_ptr_cast",
        );
        compiler.builder.build_pointer_cast(
            compiler
                .builder
                .build_call(
                    cons_fn,
                    &[head_ptr.into(), tail.into(), element_type_len],
                    "cons_fn__",
                )
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_pointer_value(),
            head.get_type(),
            "list_cons__ptr",
        )
    }

    pub fn tail<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, TonyError> {
        let head_fn = compiler.get_function("list_tail__").unwrap();

        Ok(compiler
            .builder
            .build_pointer_cast(
                compiler
                    .builder
                    .build_call(head_fn, &[ptr.into()], "tail_fn__")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value(),
                ptr.get_type(),
                "list_tail__ptr",
            )
            .into())
    }

    pub fn head<'a, 'ctx>(
        compiler: &'_ Compiler<'a, 'ctx>,
        ptr: PointerValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, TonyError> {
        let head_fn = compiler.get_function("list_head__").unwrap();

        Ok(compiler
            .builder
            .build_pointer_cast(
                compiler
                    .builder
                    .build_call(head_fn, &[ptr.into()], "head_fn__")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value(),
                ptr.get_type(),
                "list_head__ptr",
            )
            .into())
    }
}
