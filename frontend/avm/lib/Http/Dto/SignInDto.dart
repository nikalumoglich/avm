class SignInDto {
  SignInDto(this.email, this.password);

  String email;
  String password;

  SignInDto.fromJson(Map<String, dynamic> json)
      : email = json['email'],
        password = json['password'];

  Map<String, dynamic> toJson() => {
    'email': email,
    'password': password,
  };
}