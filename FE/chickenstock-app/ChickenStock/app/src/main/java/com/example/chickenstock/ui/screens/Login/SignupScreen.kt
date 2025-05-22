package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.Visibility
import androidx.compose.material.icons.filled.VisibilityOff
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.ImeAction
import com.example.chickenstock.ui.theme.Gray300
import com.example.chickenstock.ui.theme.Gray500
import com.example.chickenstock.ui.theme.Gray700
import androidx.compose.ui.focus.onFocusChanged
import androidx.activity.compose.BackHandler
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.EmailCheckRequest
import com.example.chickenstock.api.RetrofitClient
import kotlinx.coroutines.launch
import androidx.compose.runtime.rememberCoroutineScope
import com.example.chickenstock.api.NicknameCheckRequest
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.platform.LocalContext

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SignupScreen(navController: NavController) {
    var email by remember { mutableStateOf("") }
    var name by remember { mutableStateOf("") }
    var nickname by remember { mutableStateOf("") }
    var password by remember { mutableStateOf("") }
    var passwordConfirm by remember { mutableStateOf("") }
    
    // 경고 대화상자 표시 상태
    var showExitDialog by remember { mutableStateOf(false) }
    var isLoading by remember { mutableStateOf(false) }
    
    // 유효성 검사 상태
    var isEmailValid by remember { mutableStateOf(true) }
    var isNicknameValid by remember { mutableStateOf(true) }
    var isPasswordMatching by remember { mutableStateOf(true) }
    
    // 빈 필드 상태 추적
    var isEmailEmpty by remember { mutableStateOf(false) }
    var isNameEmpty by remember { mutableStateOf(false) }
    var isNicknameEmpty by remember { mutableStateOf(false) }
    var isPasswordEmpty by remember { mutableStateOf(false) }
    var isPasswordConfirmEmpty by remember { mutableStateOf(false) }
    
    // 포커스 상태 추적
    var emailFocused by remember { mutableStateOf(false) }
    var nicknameFocused by remember { mutableStateOf(false) }
    var passwordFocused by remember { mutableStateOf(false) }
    var passwordConfirmFocused by remember { mutableStateOf(false) }
    
    // 이메일 검증 상태
    var isEmailAvailable by remember { mutableStateOf<Boolean?>(null) }
    var emailCheckMessage by remember { mutableStateOf<String?>(null) }
    
    // 닉네임 검증 상태
    var isNicknameAvailable by remember { mutableStateOf<Boolean?>(null) }
    var nicknameCheckMessage by remember { mutableStateOf<String?>(null) }
    
    // 비밀번호 표시 상태
    var passwordVisible by remember { mutableStateOf(false) }
    var passwordConfirmVisible by remember { mutableStateOf(false) }
    
    // 비밀번호 입력란 상호작용 소스
    val passwordInteractionSource = remember { MutableInteractionSource() }
    val passwordConfirmInteractionSource = remember { MutableInteractionSource() }
    
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }

    // 사용자 입력이 있는지 확인하는 함수
    fun hasUserInput(): Boolean {
        return email.isNotEmpty() || name.isNotEmpty() || nickname.isNotEmpty() || 
               password.isNotEmpty() || passwordConfirm.isNotEmpty()
    }
    
    // 뒤로가기 처리
    fun handleBackNavigation() {
        if (hasUserInput()) {
            showExitDialog = true
        } else {
            navController.navigateUp()
        }
    }
    
    // 시스템 뒤로가기 처리
    BackHandler {
        handleBackNavigation()
    }
    
    // 경고 대화상자
    if (showExitDialog) {
        AlertDialog(
            onDismissRequest = { showExitDialog = false },
            title = { Text("회원가입 취소", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("입력된 정보가 모두 초기화됩니다. 정말 나가시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(
                    onClick = {
                        showExitDialog = false
                        navController.navigateUp()
                    }
                ) {
                    Text("나가기", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showExitDialog = false }
                ) {
                    Text("계속 작성하기", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }
    
    // 이메일 유효성 검사 함수
    fun validateEmail(email: String): Boolean {
        if (email.isEmpty()) return true
        val emailRegex = "[a-zA-Z0-9._-]+@[a-z]+\\.+[a-z]+".toRegex()
        return email.matches(emailRegex)
    }
    
    // 닉네임 유효성 검사 함수
    fun validateNickname(nickname: String): Boolean {
        if (nickname.isEmpty()) return true
        val nicknameRegex = "[가-힣a-zA-Z0-9]{1,10}".toRegex()
        return nickname.matches(nicknameRegex)
    }
    
    // 비밀번호 일치 검사 함수
    fun validatePasswordsMatch(): Boolean {
        if (passwordConfirm.isEmpty()) return true
        return password == passwordConfirm
    }
    
    // 비밀번호 필드 유효성 검사 수행
    fun checkPasswordMatching() {
        isPasswordMatching = validatePasswordsMatch()
    }
    
    // 필수 입력값 검사
    fun checkRequiredFields() {
        isEmailEmpty = email.isEmpty()
        isNameEmpty = name.isEmpty()
        isNicknameEmpty = nickname.isEmpty()
        isPasswordEmpty = password.isEmpty()
        isPasswordConfirmEmpty = passwordConfirm.isEmpty()
        
        // 이메일과 닉네임이 비어있지 않다면 유효성 검사 수행
        if (!isEmailEmpty) isEmailValid = validateEmail(email)
        if (!isNicknameEmpty) isNicknameValid = validateNickname(nickname)
        if (!isPasswordEmpty && !isPasswordConfirmEmpty) isPasswordMatching = validatePasswordsMatch()
    }
    
    // 비밀번호 유효성 검사 함수 추가
    fun validatePassword(password: String): Boolean {
        return password.length >= 8
    }
    
    // 이메일 중복 확인 함수 수정
    fun checkEmailAvailability(email: String) {
        if (!validateEmail(email)) {
            isEmailValid = false
            return
        }
        
        coroutineScope.launch {
            try {
                println("이메일 중복 확인 시도: $email")
                val response = authService.checkEmail(EmailCheckRequest(email))
                println("이메일 중복 확인 응답 코드: ${response.code()}")
                
                if (response.isSuccessful) {
                    val result = response.body()
                    println("이메일 중복 확인 응답: ${result?.message}")
                    isEmailAvailable = result?.success
                    emailCheckMessage = if (result?.success == true) {
                        "사용 가능한 이메일입니다"
                    } else {
                        "중복된 이메일입니다"
                    }
                } else {
                    isEmailAvailable = false
                    emailCheckMessage = if (response.code() == 400) {
                        "중복된 이메일입니다"
                    } else {
                        "서버 오류가 발생했습니다 (${response.code()})"
                    }
                    println("이메일 중복 확인 실패: ${response.code()}")
                }
            } catch (e: Exception) {
                println("이메일 중복 확인 오류: ${e.message}")
                isEmailAvailable = false
                emailCheckMessage = when {
                    e.message?.contains("Unable to resolve host") == true -> 
                        "서버에 연결할 수 없습니다. 인터넷 연결을 확인해주세요."
                    e.message?.contains("timeout") == true -> 
                        "서버 응답이 지연되고 있습니다. 잠시 후 다시 시도해주세요."
                    else -> "네트워크 오류가 발생했습니다. 잠시 후 다시 시도해주세요."
                }
            }
        }
    }
    
    // 닉네임 중복 확인 함수
    fun checkNicknameAvailability(nickname: String) {
        if (!validateNickname(nickname)) {
            isNicknameValid = false
            return
        }
        
        coroutineScope.launch {
            try {
                println("닉네임 중복 확인 시도: $nickname") // 디버그 로그
                val response = authService.checkNickname(NicknameCheckRequest(nickname))
                println("닉네임 중복 확인 응답 코드: ${response.code()}") // 디버그 로그
                
                if (response.isSuccessful) {
                    val result = response.body()
                    println("닉네임 중복 확인 응답: ${result?.message}") // 디버그 로그
                    isNicknameAvailable = result?.duplicate?.let { !it }
                    nicknameCheckMessage = result?.message
                } else {
                    isNicknameAvailable = false
                    nicknameCheckMessage = "서버 오류가 발생했습니다 (${response.code()})"
                    println("닉네임 중복 확인 실패: ${response.code()}") // 디버그 로그
                }
            } catch (e: Exception) {
                println("닉네임 중복 확인 오류: ${e.message}") // 디버그 로그
                isNicknameAvailable = false
                nicknameCheckMessage = when {
                    e.message?.contains("Unable to resolve host") == true -> 
                        "서버에 연결할 수 없습니다. 인터넷 연결을 확인해주세요."
                    e.message?.contains("timeout") == true -> 
                        "서버 응답이 지연되고 있습니다. 잠시 후 다시 시도해주세요."
                    else -> "네트워크 오류가 발생했습니다. 잠시 후 다시 시도해주세요."
                }
            }
        }
    }
    
    Box(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
    ) {
        // 뒤로가기 버튼
        IconButton(
            onClick = { handleBackNavigation() },
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(16.dp)
        ) {
            Icon(
                imageVector = Icons.Filled.KeyboardArrowLeft,
                contentDescription = "뒤로가기",
                modifier = Modifier.size(32.dp),
                tint = Color.Black
            )
        }
        
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 24.dp)
                .align(Alignment.Center),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            // 회원가입 타이틀
            Text(
                text = "회원가입",
                fontSize = 24.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                textAlign = TextAlign.Center,
                modifier = Modifier.padding(bottom = 32.dp)
            )
            
            // 이메일 입력 필드
            OutlinedTextField(
                value = email,
                onValueChange = { 
                    email = it
                    isEmailEmpty = false
                    isEmailAvailable = null // 이메일이 변경되면 검증 상태 초기화
                    emailCheckMessage = null
                },
                label = { Text("이메일", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (isEmailValid && !isEmailEmpty && emailCheckMessage == null) 12.dp else 4.dp)
                    .onFocusChanged { 
                        val wasFocused = emailFocused
                        emailFocused = it.isFocused
                        if (wasFocused && !emailFocused && email.isNotEmpty()) {
                            // 포커스가 빠져나갈 때 이메일 형식 확인 후 중복 확인
                            isEmailValid = validateEmail(email)
                            if (isEmailValid) {
                                checkEmailAvailability(email)
                            }
                        }
                    },
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = when {
                        !isEmailValid || isEmailEmpty -> Color.Red
                        isEmailAvailable == true -> Color(0xFF4CAF50)
                        else -> Gray300
                    },
                    focusedBorderColor = when {
                        !isEmailValid || isEmailEmpty -> Color.Red
                        isEmailAvailable == true -> Color(0xFF4CAF50)
                        else -> Gray300
                    },
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Email,
                    imeAction = ImeAction.Next
                ),
                isError = !isEmailValid || isEmailEmpty || isEmailAvailable == false
            )
            
            // 이메일 상태 메시지
            if (!isEmailValid || isEmailEmpty || emailCheckMessage != null) {
                Text(
                    text = when {
                        isEmailEmpty -> "이메일을 입력해주세요"
                        !isEmailValid -> "이메일 형식이 맞지 않습니다"
                        else -> emailCheckMessage ?: ""
                    },
                    color = when {
                        !isEmailValid || isEmailEmpty -> Color.Red
                        isEmailAvailable == true -> Color(0xFF4CAF50)
                        emailCheckMessage == "서버 오류가 발생했습니다" -> Color.Red
                        else -> Gray500
                    },
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 8.dp)
                )
            }
            
            // 이름 입력 필드
            OutlinedTextField(
                value = name,
                onValueChange = { 
                    name = it
                    isNameEmpty = false // 입력 시 빈 필드 오류 초기화
                },
                label = { Text("이름", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (!isNameEmpty) 12.dp else 4.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (!isNameEmpty) Gray300 else Color.Red,
                    focusedBorderColor = if (!isNameEmpty) Gray300 else Color.Red,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                keyboardOptions = KeyboardOptions(
                    imeAction = ImeAction.Next
                ),
                isError = isNameEmpty
            )
            
            // 이름 오류 메시지
            if (isNameEmpty) {
                Text(
                    text = "이름을 입력해주세요",
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 8.dp)
                )
            }
            
            // 닉네임 입력 필드
            OutlinedTextField(
                value = nickname,
                onValueChange = { 
                    nickname = it
                    isNicknameEmpty = false
                    isNicknameAvailable = null
                    nicknameCheckMessage = null
                },
                label = { Text("닉네임", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (isNicknameValid && !isNicknameEmpty && nicknameCheckMessage == null) 4.dp else 4.dp)
                    .onFocusChanged { 
                        val wasFocused = nicknameFocused
                        nicknameFocused = it.isFocused
                        if (wasFocused && !nicknameFocused && nickname.isNotEmpty()) {
                            isNicknameValid = validateNickname(nickname)
                            if (isNicknameValid) {
                                checkNicknameAvailability(nickname)
                            }
                        }
                    },
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = when {
                        !isNicknameValid || isNicknameEmpty -> Color.Red
                        isNicknameAvailable == true -> Color(0xFF4CAF50)
                        isNicknameAvailable == false -> Color.Red
                        else -> Gray300
                    },
                    focusedBorderColor = when {
                        !isNicknameValid || isNicknameEmpty -> Color.Red
                        isNicknameAvailable == true -> Color(0xFF4CAF50)
                        isNicknameAvailable == false -> Color.Red
                        else -> Gray300
                    },
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                keyboardOptions = KeyboardOptions(
                    imeAction = ImeAction.Next
                ),
                isError = !isNicknameValid || isNicknameEmpty || isNicknameAvailable == false
            )
            
            // 닉네임 안내 및 오류 메시지
            Text(
                text = when {
                    isNicknameEmpty -> "닉네임을 입력해주세요"
                    !isNicknameValid -> "닉네임 형식이 맞지 않습니다"
                    nicknameCheckMessage != null -> nicknameCheckMessage ?: ""
                    else -> "최대 10자, 한글/영어/숫자만 사용 가능합니다"
                },
                color = when {
                    !isNicknameValid || isNicknameEmpty -> Color.Red
                    isNicknameAvailable == true -> Color(0xFF4CAF50)
                    isNicknameAvailable == false -> Color.Red
                    nicknameCheckMessage == "서버 오류가 발생했습니다" -> Color.Red
                    else -> Gray500
                },
                fontSize = 12.sp,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier
                    .align(Alignment.Start)
                    .padding(start = 8.dp, bottom = 8.dp)
            )
            
            // 비밀번호 입력 필드
            OutlinedTextField(
                value = password,
                onValueChange = { 
                    password = it
                    isPasswordEmpty = false
                    if (passwordConfirmFocused || (passwordConfirm.isNotEmpty() && !passwordFocused)) {
                        checkPasswordMatching()
                    }
                },
                label = { Text("비밀번호", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (!isPasswordEmpty && validatePassword(password)) 4.dp else 4.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = when {
                        password.isEmpty() -> Gray300
                        !validatePassword(password) -> Color.Red
                        !isPasswordMatching && passwordConfirm.isNotEmpty() -> Color.Red
                        else -> Gray300
                    },
                    focusedBorderColor = when {
                        password.isEmpty() -> Gray300
                        !validatePassword(password) -> Color.Red
                        !isPasswordMatching && passwordConfirm.isNotEmpty() -> Color.Red
                        else -> Gray300
                    },
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                visualTransformation = if (passwordVisible) VisualTransformation.None else PasswordVisualTransformation(),
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Password,
                    imeAction = ImeAction.Next
                ),
                singleLine = true,
                isError = password.isNotEmpty() && (!validatePassword(password) || (!isPasswordMatching && passwordConfirm.isNotEmpty())),
                trailingIcon = {
                    IconButton(
                        onClick = { },
                        interactionSource = passwordInteractionSource
                    ) {
                        Icon(
                            imageVector = if (passwordVisible) Icons.Default.Visibility else Icons.Default.VisibilityOff,
                            contentDescription = if (passwordVisible) "비밀번호 숨기기" else "비밀번호 보기",
                            tint = Gray500
                        )
                    }
                }
            )

            LaunchedEffect(passwordInteractionSource) {
                passwordInteractionSource.interactions.collect { interaction ->
                    passwordVisible = interaction is PressInteraction.Press
                }
            }
            
            // 비밀번호 안내 메시지
            if (password.isNotEmpty()) {
            Text(
                text = when {
                    !validatePassword(password) -> "비밀번호는 8자리 이상이어야 합니다"
                    else -> "비밀번호는 8자리 이상이어야 합니다"
                },
                color = when {
                        !validatePassword(password) -> Color.Red
                    else -> Gray500
                },
                fontSize = 12.sp,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier
                    .align(Alignment.Start)
                    .padding(start = 8.dp, bottom = 8.dp)
            )
            }
            
            // 비밀번호 재입력 필드
            OutlinedTextField(
                value = passwordConfirm,
                onValueChange = { 
                    passwordConfirm = it
                    isPasswordConfirmEmpty = false
                    if (passwordConfirm.isNotEmpty()) {
                        checkPasswordMatching()
                    }
                },
                label = { Text("비밀번호 재입력", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (isPasswordMatching && !isPasswordConfirmEmpty) 24.dp else 4.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (!isPasswordConfirmEmpty && (isPasswordMatching || passwordConfirm.isEmpty())) Gray300 else Color.Red,
                    focusedBorderColor = if (!isPasswordConfirmEmpty && (isPasswordMatching || passwordConfirm.isEmpty())) Gray300 else Color.Red,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                visualTransformation = if (passwordConfirmVisible) VisualTransformation.None else PasswordVisualTransformation(),
                keyboardOptions = KeyboardOptions(
                    keyboardType = KeyboardType.Password,
                    imeAction = ImeAction.Done
                ),
                singleLine = true,
                isError = isPasswordConfirmEmpty || (!isPasswordMatching && passwordConfirm.isNotEmpty()),
                trailingIcon = {
                    IconButton(
                        onClick = { },
                        interactionSource = passwordConfirmInteractionSource
                    ) {
                        Icon(
                            imageVector = if (passwordConfirmVisible) Icons.Default.Visibility else Icons.Default.VisibilityOff,
                            contentDescription = if (passwordConfirmVisible) "비밀번호 숨기기" else "비밀번호 보기",
                            tint = Gray500
                        )
                    }
                }
            )

            LaunchedEffect(passwordConfirmInteractionSource) {
                passwordConfirmInteractionSource.interactions.collect { interaction ->
                    passwordConfirmVisible = interaction is PressInteraction.Press
                }
            }
            
            // 비밀번호 일치 또는 빈 필드 오류 메시지
            if (!isPasswordMatching && passwordConfirm.isNotEmpty()) {
                Text(
                    text = "비밀번호가 일치하지 않습니다",
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
            } else if (isPasswordConfirmEmpty) {
                Text(
                    text = "비밀번호를 다시 한번 입력해주세요",
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
            }
            
            // 회원가입 버튼을 인증하기 버튼으로 변경
            Button(
                onClick = { 
                    // 모든 필수 입력 필드 검사
                    checkRequiredFields()
                    
                    // 모든 유효성 검사가 통과되면 인증 코드 발송
                    if (isEmailValid && isEmailAvailable == true && 
                        isNicknameValid && isNicknameAvailable == true && 
                        isPasswordMatching && 
                        !isEmailEmpty && !isNameEmpty && !isNicknameEmpty && 
                        !isPasswordEmpty && !isPasswordConfirmEmpty) {
                        
                        coroutineScope.launch {
                            isLoading = true
                            try {
                                val response = authService.sendVerificationCode(EmailCheckRequest(email))
                                if (response.isSuccessful) {
                                    // 인증 화면으로 이동
                                    val encodedEmail = email.replace("@", "__AT__")
                                    navController.navigate(
                                        "verification?email=$encodedEmail&name=$name&nickname=$nickname&password=$password"
                                    )
                                }
                            } catch (e: Exception) {
                                // 에러 처리
                            } finally {
                                isLoading = false
                            }
                        }
                    }
                },
                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = RoundedCornerShape(12.dp),
                contentPadding = PaddingValues(vertical = 12.dp),
                enabled = !isLoading
            ) {
                if (isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(20.dp),
                        color = Color.Black,
                        strokeWidth = 2.dp
                    )
                } else {
                    Text(
                        "인증하기",
                        color = Color.Black,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily,
                        fontSize = 18.sp
                    )
                }
            }
        }
    }
}
