class HttpError {
  HttpError(this.code, this.message);

  int code;
  String message;

  HttpError.fromJson(Map<String, dynamic> json)
      : code = json['code'],
        message = json['message'];

  Map<String, dynamic> toJson() => {
    'code': code,
    'message': message,
  };
}