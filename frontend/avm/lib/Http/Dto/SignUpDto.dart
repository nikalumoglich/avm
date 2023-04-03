import 'package:json_annotation/json_annotation.dart';

part 'package:avm/Http/Dto/SignUpDto.g.dart';

@JsonSerializable()
class SignUpDto {
  SignUpDto(this.name, this.email, this.password);

  String name;
  String email;
  String password;

  factory SignUpDto.fromJson(Map<String, dynamic> json) => _$SignUpDto(json);

  Map<String, dynamic> toJson() => _$SignUpDto(this);
}