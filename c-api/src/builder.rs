use frontend::FunctionBuilder;

use crate::ApiBlock;

#[no_mangle]
unsafe extern "C" fn spidir_builder_create_block(builder: *mut FunctionBuilder<'_>) -> ApiBlock {
    unsafe {
        let builder = &mut *builder;
        let block = builder.create_block();
        ApiBlock(block.as_u32())
    }
}
