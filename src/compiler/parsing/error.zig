pub const ParseError = error {
    UnexpectedEndOfTokens,
    UnexpectedToken,
    OutOfMemory,
};