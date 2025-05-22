package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
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
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.ImeAction
import com.example.chickenstock.ui.theme.Gray300
import com.example.chickenstock.ui.theme.Gray500
import androidx.activity.compose.BackHandler
import androidx.compose.foundation.clickable
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.EmailCheckRequest
import com.example.chickenstock.api.RetrofitClient
import kotlinx.coroutines.launch
import androidx.compose.runtime.rememberCoroutineScope
import com.example.chickenstock.api.VerifyCodeRequest
import androidx.compose.ui.platform.LocalContext

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun FindPWScreen(
    navController: NavController
) {
    var email by remember { mutableStateOf("") }
    var verificationCode by remember { mutableStateOf("") }
    var isVerificationSent by remember { mutableStateOf(false) }
    var isVerificationComplete by remember { mutableStateOf(false) }
    var showSuccessDialog by remember { mutableStateOf(false) }
    var isSendingCode by remember { mutableStateOf(false) }  // 인증번호 전송 로딩 상태
    var isVerifying by remember { mutableStateOf(false) }    // 인증번호 확인 로딩 상태
    
    // 이메일 유효성 검사 상태
    var isEmailValid by remember { mutableStateOf(true) }
    var isEmailEmpty by remember { mutableStateOf(false) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    
    val focusManager = LocalFocusManager.current
    val keyboardController = LocalSoftwareKeyboardController.current
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }

    // 이메일 유효성 검사 함수
    fun validateEmail(email: String): Boolean {
        if (email.isEmpty()) return false
        val emailRegex = "[a-zA-Z0-9._-]+@[a-z]+\\.+[a-z]+".toRegex()
        return email.matches(emailRegex)
    }
    
    // 뒤로가기 처리
    fun handleBackNavigation() {
        if (isVerificationSent) {
            isVerificationSent = false
            isVerificationComplete = false
        } else {
            navController.navigateUp()
        }
    }
    
    BackHandler {
        handleBackNavigation()
    }
    
    // 성공 다이얼로그
    if (showSuccessDialog) {
        AlertDialog(
            onDismissRequest = { },
            title = { 
                Text(
                    "비밀번호 찾기 완료",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold
                )
            },
            text = { 
                Text(
                    "입력하신 이메일로 임시 비밀번호가 발송되었습니다.",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            },
            confirmButton = {
                TextButton(
                    onClick = {
                        showSuccessDialog = false
                        navController.navigateUp()
                    }
                ) {
                    Text(
                        "확인",
                        color = Color(0xFF0066CC),
                        fontFamily = SCDreamFontFamily,
                        fontWeight = FontWeight.Bold
                    )
                }
            },
            containerColor = Color.White
        )
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
            // 제목
            Text(
                text = "비밀번호 찾기",
                fontSize = 24.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.padding(bottom = 32.dp)
            )
            
            if (!isVerificationSent) {
                // 이메일 입력 단계
                OutlinedTextField(
                    value = email,
                    onValueChange = { 
                        email = it
                        isEmailEmpty = false
                        isEmailValid = true
                        errorMessage = null
                    },
                    label = { Text("이메일", fontFamily = SCDreamFontFamily) },
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(bottom = if (errorMessage == null) 24.dp else 4.dp),
                    shape = RoundedCornerShape(16.dp),
                    colors = OutlinedTextFieldDefaults.colors(
                        unfocusedContainerColor = Color.White,
                        focusedContainerColor = Color.White,
                        unfocusedBorderColor = if (errorMessage == null) Gray300 else Color.Red,
                        focusedBorderColor = if (errorMessage == null) Gray300 else Color.Red,
                        unfocusedTextColor = Color.Black,
                        focusedTextColor = Color.Black,
                        errorTextColor = Color.Black
                    ),
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(
                        keyboardType = KeyboardType.Email,
                        imeAction = ImeAction.Done
                    ),
                    isError = errorMessage != null
                )

                if (errorMessage != null) {
                    Text(
                        text = errorMessage ?: "",
                        color = Color.Red,
                        fontSize = 12.sp,
                        fontFamily = SCDreamFontFamily,
                        modifier = Modifier
                            .align(Alignment.Start)
                            .padding(start = 8.dp, bottom = 24.dp)
                    )
                }
                
                Button(
                    onClick = { 
                        if (email.isEmpty()) {
                            isEmailEmpty = true
                            errorMessage = "이메일을 입력해주세요"
                            return@Button
                        }
                        
                        if (!validateEmail(email)) {
                            isEmailValid = false
                            errorMessage = "이메일 형식이 맞지 않습니다"
                            return@Button
                        }

                        coroutineScope.launch {
                            isSendingCode = true  // 로딩 시작
                            try {
                                val response = authService.sendVerificationCode(EmailCheckRequest(email))
                                if (response.isSuccessful) {
                                    isVerificationSent = true
                                    errorMessage = null
                                } else {
                                    errorMessage = "인증번호 발송에 실패했습니다"
                                }
                            } catch (e: Exception) {
                                errorMessage = "서버 오류가 발생했습니다"
                            } finally {
                                isSendingCode = false  // 로딩 종료
                            }
                        }
                    },
                    colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(50.dp),
                    shape = RoundedCornerShape(12.dp),
                    contentPadding = PaddingValues(vertical = 12.dp),
                    enabled = !isSendingCode  // 로딩 중에는 버튼 비활성화
                ) {
                    if (isSendingCode) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(20.dp),
                            color = Color.Black,
                            strokeWidth = 2.dp
                        )
                    } else {
                        Text(
                            "인증번호 보내기",
                            color = Color.Black,
                            fontWeight = FontWeight.Bold,
                            fontFamily = SCDreamFontFamily,
                            fontSize = 18.sp
                        )
                    }
                }
            } else if (!isVerificationComplete) {
                // 인증번호 입력 단계
                OutlinedTextField(
                    value = verificationCode,
                    onValueChange = { 
                        verificationCode = it
                        errorMessage = null
                    },
                    label = { Text("인증번호", fontFamily = SCDreamFontFamily) },
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(bottom = if (errorMessage == null) 24.dp else 4.dp),
                    shape = RoundedCornerShape(16.dp),
                    colors = OutlinedTextFieldDefaults.colors(
                        unfocusedContainerColor = Color.White,
                        focusedContainerColor = Color.White,
                        unfocusedBorderColor = if (errorMessage == null) Gray300 else Color.Red,
                        focusedBorderColor = if (errorMessage == null) Gray300 else Color.Red,
                        unfocusedTextColor = Color.Black,
                        focusedTextColor = Color.Black,
                        errorTextColor = Color.Black
                    ),
                    singleLine = true,
                    keyboardOptions = KeyboardOptions(
                        keyboardType = KeyboardType.Number,
                        imeAction = ImeAction.Done
                    ),
                    isError = errorMessage != null
                )

                if (errorMessage != null) {
                    Text(
                        text = errorMessage ?: "",
                        color = Color.Red,
                        fontSize = 12.sp,
                        fontFamily = SCDreamFontFamily,
                        modifier = Modifier
                            .align(Alignment.Start)
                            .padding(start = 8.dp, bottom = 24.dp)
                    )
                }
                
                Button(
                    onClick = { 
                        if (verificationCode.isEmpty()) {
                            errorMessage = "인증번호를 입력해주세요"
                            return@Button
                        }

                        coroutineScope.launch {
                            isVerifying = true  // 로딩 시작
                            try {
                                // 인증번호 확인
                                val verifyResponse = authService.verifyCode(VerifyCodeRequest(email, verificationCode))
                                if (verifyResponse.isSuccessful && verifyResponse.body()?.success == true) {
                                    // 임시 비밀번호 발급 요청
                                    val resetResponse = authService.resetPasswordByCode(EmailCheckRequest(email))
                                    if (resetResponse.isSuccessful) {
                                        isVerificationComplete = true
                                        showSuccessDialog = true
                                        errorMessage = null
                                    } else {
                                        errorMessage = "임시 비밀번호 발급에 실패했습니다"
                                    }
                                } else {
                                    errorMessage = "인증번호가 일치하지 않습니다"
                                }
                            } catch (e: Exception) {
                                errorMessage = "서버 오류가 발생했습니다"
                            } finally {
                                isVerifying = false  // 로딩 종료
                            }
                        }
                    },
                    colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(50.dp),
                    shape = RoundedCornerShape(12.dp),
                    contentPadding = PaddingValues(vertical = 12.dp),
                    enabled = !isVerifying  // 로딩 중에는 버튼 비활성화
                ) {
                    if (isVerifying) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(20.dp),
                            color = Color.Black,
                            strokeWidth = 2.dp
                        )
                    } else {
                        Text(
                            "인증번호 확인",
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
}
