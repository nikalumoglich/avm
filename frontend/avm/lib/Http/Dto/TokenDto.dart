class Token {
  Token(this.token);

  String token;

  Token.fromJson(Map<String, dynamic> json)
      : token = json['token'];

  Map<String, dynamic> toJson() => {
    'token': token,
  };
}