use std::ffi::CStr;
use std::os::raw::c_char;
use tiktoken_rs::{get_bpe_from_model, CoreBPE};

/// Error codes for FFI
#[repr(C)]
pub enum TokenizerError {
    Success = 0,
    InvalidModel = 1,
    InvalidUtf8 = 2,
    EncodingError = 3,
    NullPointer = 4,
}

/// Get the BPE encoder for a given model name
fn get_encoder(model_name: &str) -> Result<CoreBPE, TokenizerError> {
    get_bpe_from_model(model_name).map_err(|_| TokenizerError::InvalidModel)
}

/// Count tokens for a given model and text
///
/// # Arguments
/// * `model_name` - C string containing the model name (e.g., "gpt-4", "gpt-3.5-turbo")
/// * `text` - C string containing the text to tokenize
/// * `out_count` - Pointer to write the token count to
///
/// # Returns
/// Error code (0 = success)
///
/// # Safety
/// This function is unsafe because it dereferences raw pointers.
/// Callers must ensure:
/// - `model_name` and `text` are valid, null-terminated C strings
/// - `out_count` is a valid pointer to a u32
#[no_mangle]
pub unsafe extern "C" fn tokenizer_count_tokens(
    model_name: *const c_char,
    text: *const c_char,
    out_count: *mut u32,
) -> TokenizerError {
    // Validate pointers
    if model_name.is_null() || text.is_null() || out_count.is_null() {
        return TokenizerError::NullPointer;
    }

    // Convert C strings to Rust strings
    let model_cstr = match CStr::from_ptr(model_name).to_str() {
        Ok(s) => s,
        Err(_) => return TokenizerError::InvalidUtf8,
    };

    let text_cstr = match CStr::from_ptr(text).to_str() {
        Ok(s) => s,
        Err(_) => return TokenizerError::InvalidUtf8,
    };

    // Get encoder for the model
    let encoder = match get_encoder(model_cstr) {
        Ok(enc) => enc,
        Err(e) => return e,
    };

    // Encode the text
    let tokens = encoder.encode_with_special_tokens(text_cstr);

    // Write the count
    *out_count = tokens.len() as u32;

    TokenizerError::Success
}

/// Check if a model is supported
///
/// # Arguments
/// * `model_name` - C string containing the model name
///
/// # Returns
/// 1 if supported, 0 if not supported or error
///
/// # Safety
/// This function is unsafe because it dereferences a raw pointer.
/// Callers must ensure `model_name` is a valid, null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn tokenizer_is_model_supported(
    model_name: *const c_char,
) -> i32 {
    if model_name.is_null() {
        return 0;
    }

    let model_cstr = match CStr::from_ptr(model_name).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Try to get the encoder
    match get_encoder(model_cstr) {
        Ok(_) => 1,
        Err(_) => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_count_tokens_gpt4() {
        let model = CString::new("gpt-4").unwrap();
        let text = CString::new("Hello, world!").unwrap();
        let mut count: u32 = 0;

        unsafe {
            let result = tokenizer_count_tokens(
                model.as_ptr(),
                text.as_ptr(),
                &mut count as *mut u32,
            );

            assert_eq!(result as i32, TokenizerError::Success as i32);
            assert!(count > 0);
            println!("Token count for 'Hello, world!': {}", count);
        }
    }

    #[test]
    fn test_is_model_supported() {
        let gpt4 = CString::new("gpt-4").unwrap();
        let invalid = CString::new("invalid-model").unwrap();

        unsafe {
            assert_eq!(tokenizer_is_model_supported(gpt4.as_ptr()), 1);
            assert_eq!(tokenizer_is_model_supported(invalid.as_ptr()), 0);
        }
    }

    #[test]
    fn test_null_pointer_handling() {
        let mut count: u32 = 0;

        unsafe {
            let result = tokenizer_count_tokens(
                std::ptr::null(),
                std::ptr::null(),
                &mut count as *mut u32,
            );

            assert_eq!(result as i32, TokenizerError::NullPointer as i32);
        }
    }
}
