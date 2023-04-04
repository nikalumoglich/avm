class SignUpDto {
  SignUpDto(this.name, this.email, this.password);

  String name;
  String email;
  String password;

  SignUpDto.fromJson(Map<String, dynamic> json)
      : name = json['name'],
        email = json['email'],
        password = json['password'];

  Map<String, dynamic> toJson() => {
    'name': name,
    'email': email,
    'password': password,
  };
}