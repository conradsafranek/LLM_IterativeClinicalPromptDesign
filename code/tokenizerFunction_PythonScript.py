
import tiktoken

# Define the encoding for the tokenizer
# Both gpt-3.5-turbo and gpt-4 use the cl100k_base encoding
encoding_name = "cl100k_base"
encoding = tiktoken.get_encoding(encoding_name)

#Define tokenizer function "num_tokens_from_string" that can 
def num_tokens_from_string(string: str) -> int:
    """Returns the number of tokens in a text string."""
    num_tokens = len(encoding.encode(string))
    return num_tokens
