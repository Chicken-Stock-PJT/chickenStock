package com.example.chickenstock.ui.screens.mypage

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.material.icons.filled.Visibility
import androidx.compose.material.icons.filled.VisibilityOff
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
import com.example.chickenstock.ui.theme.*
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.viewmodel.AuthViewModel
import androidx.lifecycle.viewmodel.compose.viewModel
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.api.AuthService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.data.TokenManager
import androidx.compose.ui.platform.LocalContext
import kotlinx.coroutines.launch
import androidx.compose.ui.text.input.PasswordVisualTransformation
import com.example.chickenstock.api.ChangePasswordRequest
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.PressInteraction
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.text.input.VisualTransformation
import okhttp3.ResponseBody
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.Header
import retrofit2.http.PATCH
import retrofit2.http.POST

// API 서비스에 닉네임 변경 및 자본 초기화 요청 추가
interface MemberUpdateService {
    @PATCH("members/nickname")
    suspend fun changeNickname(
        @Header("Authorization") token: String,
        @Body nicknameRequest: NicknameRequest
    ): Response<ResponseBody>
    
    @POST("members/initialize-money")
    suspend fun initializeMoney(
        @Header("Authorization") token: String
    ): Response<ResponseBody>
}

// 닉네임 변경 요청을 위한 데이터 클래스
data class NicknameRequest(val nickname: String)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SettingScreen(
    navController: NavController,
    viewModel: MainViewModel,
    authViewModel: AuthViewModel = viewModel()
) {
    var showLogoutDialog by remember { mutableStateOf(false) }
    var showInitializeDialog by remember { mutableStateOf(false) }
    var isLoading by remember { mutableStateOf(false) }
    var showChangePassword by remember { mutableStateOf(false) }
    var showChangeNickname by remember { mutableStateOf(false) }
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }
    val memberUpdateService = remember { RetrofitClient.getInstance(context).create(MemberUpdateService::class.java) }
    val tokenManager = remember { TokenManager.getInstance(context) }

    // 로그아웃 확인 다이얼로그
    if (showLogoutDialog) {
        AlertDialog(
            onDismissRequest = { showLogoutDialog = false },
            title = { Text("로그아웃", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("정말 로그아웃 하시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(
                    onClick = {
                        coroutineScope.launch {
                            isLoading = true
                            try {
                                println("로그아웃 시도")
                                val response = authService.logout()
                                println("로그아웃 응답 코드: ${response.code()}")
                                println("로그아웃 응답 메시지: ${response.message()}")
                                println("로그아웃 응답 헤더: ${response.headers()}")
                                
                                if (response.isSuccessful) {
                                    println("로그아웃 성공")
                                    tokenManager.clearTokens()
                                    showLogoutDialog = false
                                    authViewModel.logout()
                                    viewModel.updateSelectedIndex(0)  // 하단 네비바를 홈으로 변경
                                    navController.navigate(Screen.Home.route) {
                                        popUpTo(navController.graph.startDestinationId) {
                                            inclusive = true
                                        }
                                    }
                                } else {
                                    println("로그아웃 실패: ${response.code()}")
                                    val errorBody = response.errorBody()?.string()
                                    println("에러 응답: $errorBody")
                                    
                                    // 서버 응답이 실패해도 로컬에서 로그아웃 처리
                                    tokenManager.clearTokens()
                                    showLogoutDialog = false
                                    authViewModel.logout()
                                    viewModel.updateSelectedIndex(0)
                                    navController.navigate(Screen.Home.route) {
                                        popUpTo(navController.graph.startDestinationId) {
                                            inclusive = true
                                        }
                                    }
                                }
                            } catch (e: Exception) {
                                println("로그아웃 오류: ${e.message}")
                                // 에러가 발생해도 로그아웃 처리
                                tokenManager.clearTokens()
                                showLogoutDialog = false
                                authViewModel.logout()
                                viewModel.updateSelectedIndex(0)
                                navController.navigate(Screen.Home.route) {
                                    popUpTo(navController.graph.startDestinationId) {
                                        inclusive = true
                                    }
                                }
                            } finally {
                                isLoading = false
                            }
                        }
                    }
                ) {
                    if (isLoading) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(16.dp),
                            color = Color.Red,
                            strokeWidth = 2.dp
                        )
                    } else {
                        Text("로그아웃", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                    }
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showLogoutDialog = false },
                    enabled = !isLoading
                ) {
                    Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }

    // 자본 초기화 확인 다이얼로그
    if (showInitializeDialog) {
        AlertDialog(
            onDismissRequest = { showInitializeDialog = false },
            title = { Text("자본 초기화", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("정말 자본을 1억원으로 초기화하시겠습니까?\n모든 주식 포지션이 정리되고 현금이 1억원으로 초기화됩니다.", fontFamily = SCDreamFontFamily, color = Color.Black, textAlign = TextAlign.Center) },
            confirmButton = {
                TextButton(
                    onClick = {
                        coroutineScope.launch {
                            isLoading = true
                            try {
                                val token = tokenManager.getAccessToken()
                                if (token != null) {
                                    val response = memberUpdateService.initializeMoney("Bearer $token")
                                    if (response.isSuccessful) {
                                        // 성공 메시지 표시
                                        showInitializeDialog = false
                                        // 홈 화면으로 이동
                                        navController.navigate(Screen.Home.route) {
                                            popUpTo(navController.graph.startDestinationId)
                                        }
                                    } else {
                                        println("자본 초기화 실패: ${response.code()}")
                                        val errorBody = response.errorBody()?.string()
                                        println("에러 응답: $errorBody")
                                    }
                                }
                            } catch (e: Exception) {
                                println("자본 초기화 오류: ${e.message}")
                            } finally {
                                isLoading = false
                                showInitializeDialog = false
                            }
                        }
                    }
                ) {
                    if (isLoading) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(16.dp),
                            color = Color.Red,
                            strokeWidth = 2.dp
                        )
                    } else {
                        Text("초기화", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                    }
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showInitializeDialog = false },
                    enabled = !isLoading
                ) {
                    Text("취소", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }

    if (showChangePassword) {
        ChangePasswordScreen(
            onBack = { showChangePassword = false },
            viewModel = viewModel
        )
    } else if (showChangeNickname) {
        ChangeNicknameScreen(
            onBack = { showChangeNickname = false },
            viewModel = viewModel
        )
    } else {
        // 설정 화면 진입 시 탑바와 하단바 숨기기
        LaunchedEffect(Unit) {
            viewModel.setTopBarVisibility(false)
            viewModel.setBottomBarVisibility(false)
        }

        // 화면을 나갈 때 탑바와 하단바 다시 보이게 하기
        DisposableEffect(Unit) {
            onDispose {
                viewModel.setTopBarVisibility(true)
                viewModel.setBottomBarVisibility(true)
            }
        }

        Scaffold(
            containerColor = Gray0,
            topBar = {
                TopAppBar(
                    title = { 
                        Text(
                            text = "설정",
                            fontSize = 24.sp,
                            fontWeight = FontWeight.Bold,
                            fontFamily = SCDreamFontFamily,
                            color = Gray700,
                            modifier = Modifier.offset(y = 3.dp)
                        )
                    },
                    navigationIcon = {
                        IconButton(
                            onClick = { navController.navigateUp() },
                            modifier = Modifier
                                .size(72.dp)
                                .padding(start = 0.dp)
                        ) {
                            Icon(
                                imageVector = Icons.Filled.KeyboardArrowLeft,
                                contentDescription = "뒤로가기",
                                modifier = Modifier.size(52.dp),
                                tint = Gray700
                            )
                        }
                    },
                    colors = TopAppBarDefaults.topAppBarColors(
                        containerColor = Gray0
                    ),
                    modifier = Modifier.height(64.dp)
                )
            }
        ) { paddingValues ->
            Column(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(paddingValues)
                    .padding(horizontal = 16.dp)
            ) {
                // 설정 메뉴 아이템들
                SettingMenuItem(
                    title = "비밀번호 변경",
                    onClick = { showChangePassword = true }
                )
                SettingMenuItem(
                    title = "닉네임 변경",
                    onClick = { showChangeNickname = true }
                )
                SettingMenuItem(
                    title = "자본 초기화",
                    onClick = { showInitializeDialog = true }
                )
                SettingMenuItem(
                    title = "로그아웃",
                    onClick = { showLogoutDialog = true }
                )
            }
        }
    }
}

@Composable
fun SettingMenuItem(
    title: String,
    onClick: () -> Unit
) {
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 8.dp),
        shape = RoundedCornerShape(12.dp),
        color = Color.White,
        onClick = onClick
    ) {
        Text(
            text = title,
            fontSize = 16.sp,
            fontFamily = SCDreamFontFamily,
            color = Gray700,
            modifier = Modifier.padding(16.dp)
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ChangePasswordScreen(
    onBack: () -> Unit,
    viewModel: MainViewModel = viewModel()
) {
    var currentPassword by remember { mutableStateOf("") }
    var newPassword by remember { mutableStateOf("") }
    var checkPassword by remember { mutableStateOf("") }
    var isLoading by remember { mutableStateOf(false) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    var showSuccessDialog by remember { mutableStateOf(false) }
    
    // 비밀번호 표시 상태
    var currentPasswordVisible by remember { mutableStateOf(false) }
    var newPasswordVisible by remember { mutableStateOf(false) }
    var checkPasswordVisible by remember { mutableStateOf(false) }
    
    // 비밀번호 입력란 상호작용 소스
    val currentPasswordInteractionSource = remember { MutableInteractionSource() }
    val newPasswordInteractionSource = remember { MutableInteractionSource() }
    val checkPasswordInteractionSource = remember { MutableInteractionSource() }
    
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val authService = remember { RetrofitClient.getInstance(context).create(AuthService::class.java) }
    val tokenManager = remember { TokenManager.getInstance(context) }

    // 하단 바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setBottomBarVisibility(false)
    }

    // 화면을 나갈 때 하단 바 다시 보이게 하기
    DisposableEffect(Unit) {
        onDispose {
            viewModel.setBottomBarVisibility(true)
        }
    }

    if (showSuccessDialog) {
        AlertDialog(
            onDismissRequest = { },
            title = { 
                Text(
                    "비밀번호 변경 완료",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold
                )
            },
            text = { 
                Text(
                    "비밀번호가 성공적으로 변경되었습니다.",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            },
            confirmButton = {
                TextButton(onClick = onBack) {
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

    Scaffold(
        containerColor = Gray0,
        topBar = {
            TopAppBar(
                title = { 
                    Text(
                        text = "비밀번호 변경",
                        fontSize = 24.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily,
                        color = Gray700,
                        modifier = Modifier.offset(y = 3.dp)
                    )
                },
                navigationIcon = {
                    IconButton(
                        onClick = onBack,
                        modifier = Modifier
                            .size(72.dp)
                            .padding(start = 0.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Filled.KeyboardArrowLeft,
                            contentDescription = "뒤로가기",
                            modifier = Modifier.size(52.dp),
                            tint = Gray700
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                ),
                modifier = Modifier.height(64.dp)
            )
        }
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(paddingValues)
                .padding(horizontal = 24.dp),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            Spacer(modifier = Modifier.height(32.dp))

            // 현재 비밀번호
            OutlinedTextField(
                value = currentPassword,
                onValueChange = { 
                    currentPassword = it
                    errorMessage = null
                },
                label = { Text("현재 비밀번호", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 16.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                visualTransformation = if (currentPasswordVisible) VisualTransformation.None else PasswordVisualTransformation(),
                trailingIcon = {
                    IconButton(
                        onClick = { },
                        interactionSource = currentPasswordInteractionSource
                    ) {
                        Icon(
                            imageVector = if (currentPasswordVisible) Icons.Default.Visibility else Icons.Default.VisibilityOff,
                            contentDescription = if (currentPasswordVisible) "비밀번호 숨기기" else "비밀번호 보기",
                            tint = Gray500
                        )
                    }
                }
            )

            LaunchedEffect(currentPasswordInteractionSource) {
                currentPasswordInteractionSource.interactions.collect { interaction ->
                    if (interaction is PressInteraction.Press) {
                        currentPasswordVisible = !currentPasswordVisible
                    }
                }
            }

            // 새 비밀번호
            OutlinedTextField(
                value = newPassword,
                onValueChange = { 
                    newPassword = it
                    errorMessage = null
                },
                label = { Text("새 비밀번호", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 16.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                visualTransformation = if (newPasswordVisible) VisualTransformation.None else PasswordVisualTransformation(),
                trailingIcon = {
                    IconButton(
                        onClick = { },
                        interactionSource = newPasswordInteractionSource
                    ) {
                        Icon(
                            imageVector = if (newPasswordVisible) Icons.Default.Visibility else Icons.Default.VisibilityOff,
                            contentDescription = if (newPasswordVisible) "비밀번호 숨기기" else "비밀번호 보기",
                            tint = Gray500
                        )
                    }
                }
            )

            LaunchedEffect(newPasswordInteractionSource) {
                newPasswordInteractionSource.interactions.collect { interaction ->
                    if (interaction is PressInteraction.Press) {
                        newPasswordVisible = !newPasswordVisible
                    }
                }
            }

            // 새 비밀번호 확인
            OutlinedTextField(
                value = checkPassword,
                onValueChange = { 
                    checkPassword = it
                    errorMessage = null
                },
                label = { Text("새 비밀번호 확인", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (errorMessage != null) 8.dp else 24.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true,
                visualTransformation = if (checkPasswordVisible) VisualTransformation.None else PasswordVisualTransformation(),
                trailingIcon = {
                    IconButton(
                        onClick = { },
                        interactionSource = checkPasswordInteractionSource
                    ) {
                        Icon(
                            imageVector = if (checkPasswordVisible) Icons.Default.Visibility else Icons.Default.VisibilityOff,
                            contentDescription = if (checkPasswordVisible) "비밀번호 숨기기" else "비밀번호 보기",
                            tint = Gray500
                        )
                    }
                }
            )

            LaunchedEffect(checkPasswordInteractionSource) {
                checkPasswordInteractionSource.interactions.collect { interaction ->
                    if (interaction is PressInteraction.Press) {
                        checkPasswordVisible = !checkPasswordVisible
                    }
                }
            }

            // 에러 메시지
            if (errorMessage != null) {
                Text(
                    text = errorMessage!!,
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
            }

            // 변경하기 버튼
            Button(
                onClick = {
                    if (currentPassword.isEmpty() || newPassword.isEmpty() || checkPassword.isEmpty()) {
                        errorMessage = "모든 필드를 입력해주세요"
                        return@Button
                    }

                    if (newPassword.length < 8) {
                        errorMessage = "새 비밀번호는 8자리 이상이어야 합니다"
                        return@Button
                    }

                    if (newPassword != checkPassword) {
                        errorMessage = "새 비밀번호가 일치하지 않습니다"
                        return@Button
                    }

                    coroutineScope.launch {
                        isLoading = true
                        try {
                            println("비밀번호 변경 시도")
                            println("현재 비밀번호: $currentPassword")
                            println("새 비밀번호: $newPassword")
                            println("확인 비밀번호: $checkPassword")
                            
                            val accessToken = tokenManager.getAccessToken()
                            if (accessToken == null) {
                                errorMessage = "로그인이 필요합니다"
                                return@launch
                            }

                            val response = authService.changePassword(
                                "Bearer $accessToken",
                                ChangePasswordRequest(
                                    currentPassword = currentPassword,
                                    newPassword = newPassword,
                                    checkPassword = checkPassword
                                )
                            )
                            println("비밀번호 변경 응답 코드: ${response.code()}")
                            
                            if (response.isSuccessful) {
                                println("비밀번호 변경 성공")
                                showSuccessDialog = true
                            } else {
                                println("비밀번호 변경 실패: ${response.code()}")
                                val errorBody = response.errorBody()?.string()
                                println("에러 응답: $errorBody")
                                
                                errorMessage = when (response.code()) {
                                    400 -> "비밀번호 형식이 올바르지 않습니다"
                                    401 -> "인증에 실패했습니다"
                                    404 -> "현재 비밀번호가 일치하지 않습니다"
                                    else -> "비밀번호 변경에 실패했습니다 (${response.code()})"
                                }
                            }
                        } catch (e: Exception) {
                            println("비밀번호 변경 오류: ${e.message}")
                            errorMessage = when {
                                e.message?.contains("Unable to resolve host") == true -> 
                                    "서버에 연결할 수 없습니다. 인터넷 연결을 확인해주세요."
                                e.message?.contains("timeout") == true -> 
                                    "서버 응답이 지연되고 있습니다. 잠시 후 다시 시도해주세요."
                                else -> "네트워크 오류가 발생했습니다: ${e.message}"
                            }
                        } finally {
                            isLoading = false
                        }
                    }
                },
                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = RoundedCornerShape(12.dp),
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
                        "변경하기",
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

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ChangeNicknameScreen(
    onBack: () -> Unit,
    viewModel: MainViewModel = viewModel()
) {
    var nickname by remember { mutableStateOf("") }
    var isLoading by remember { mutableStateOf(false) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    var showSuccessDialog by remember { mutableStateOf(false) }
    
    val coroutineScope = rememberCoroutineScope()
    val context = LocalContext.current
    val memberUpdateService = remember { RetrofitClient.getInstance(context).create(MemberUpdateService::class.java) }
    val tokenManager = remember { TokenManager.getInstance(context) }

    // 하단 바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setBottomBarVisibility(false)
    }

    // 화면을 나갈 때 하단 바 다시 보이게 하기
    DisposableEffect(Unit) {
        onDispose {
            viewModel.setBottomBarVisibility(true)
        }
    }

    if (showSuccessDialog) {
        AlertDialog(
            onDismissRequest = { },
            title = { 
                Text(
                    "닉네임 변경 완료",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    fontWeight = FontWeight.Bold
                )
            },
            text = { 
                Text(
                    "닉네임이 성공적으로 변경되었습니다.",
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            },
            confirmButton = {
                TextButton(onClick = onBack) {
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

    Scaffold(
        containerColor = Gray0,
        topBar = {
            TopAppBar(
                title = { 
                    Text(
                        text = "닉네임 변경",
                        fontSize = 24.sp,
                        fontWeight = FontWeight.Bold,
                        fontFamily = SCDreamFontFamily,
                        color = Gray700,
                        modifier = Modifier.offset(y = 3.dp)
                    )
                },
                navigationIcon = {
                    IconButton(
                        onClick = onBack,
                        modifier = Modifier
                            .size(72.dp)
                            .padding(start = 0.dp)
                    ) {
                        Icon(
                            imageVector = Icons.Filled.KeyboardArrowLeft,
                            contentDescription = "뒤로가기",
                            modifier = Modifier.size(52.dp),
                            tint = Gray700
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                ),
                modifier = Modifier.height(64.dp)
            )
        }
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(paddingValues)
                .padding(horizontal = 24.dp),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            Spacer(modifier = Modifier.height(32.dp))

            // 닉네임 입력 필드
            OutlinedTextField(
                value = nickname,
                onValueChange = { 
                    nickname = it
                    errorMessage = null
                },
                label = { Text("새 닉네임", fontFamily = SCDreamFontFamily) },
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = if (errorMessage != null) 8.dp else 24.dp),
                shape = RoundedCornerShape(16.dp),
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = Color.White,
                    focusedContainerColor = Color.White,
                    unfocusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    focusedBorderColor = if (errorMessage != null) Color.Red else Gray300,
                    unfocusedTextColor = Color.Black,
                    focusedTextColor = Color.Black,
                    errorTextColor = Color.Black,
                    disabledTextColor = Color.Black
                ),
                singleLine = true
            )

            // 에러 메시지
            if (errorMessage != null) {
                Text(
                    text = errorMessage!!,
                    color = Color.Red,
                    fontSize = 12.sp,
                    fontFamily = SCDreamFontFamily,
                    modifier = Modifier
                        .align(Alignment.Start)
                        .padding(start = 8.dp, bottom = 24.dp)
                )
            }

            // 변경하기 버튼
            Button(
                onClick = {
                    if (nickname.isEmpty()) {
                        errorMessage = "닉네임을 입력해주세요"
                        return@Button
                    }

                    coroutineScope.launch {
                        isLoading = true
                        try {
                            val accessToken = tokenManager.getAccessToken()
                            if (accessToken == null) {
                                errorMessage = "로그인이 필요합니다"
                                return@launch
                            }

                            val response = memberUpdateService.changeNickname(
                                "Bearer $accessToken",
                                NicknameRequest(nickname)
                            )
                            
                            if (response.isSuccessful) {
                                println("닉네임 변경 성공")
                                showSuccessDialog = true
                            } else {
                                println("닉네임 변경 실패: ${response.code()}")
                                val errorBody = response.errorBody()?.string()
                                println("에러 응답: $errorBody")
                                
                                errorMessage = when (response.code()) {
                                    400 -> "닉네임 형식이 올바르지 않습니다"
                                    401 -> "인증에 실패했습니다"
                                    409 -> "이미 사용 중인 닉네임입니다"
                                    else -> "닉네임 변경에 실패했습니다 (${response.code()})"
                                }
                            }
                        } catch (e: Exception) {
                            println("닉네임 변경 오류: ${e.message}")
                            errorMessage = when {
                                e.message?.contains("Unable to resolve host") == true -> 
                                    "서버에 연결할 수 없습니다. 인터넷 연결을 확인해주세요."
                                e.message?.contains("timeout") == true -> 
                                    "서버 응답이 지연되고 있습니다. 잠시 후 다시 시도해주세요."
                                else -> "네트워크 오류가 발생했습니다: ${e.message}"
                            }
                        } finally {
                            isLoading = false
                        }
                    }
                },
                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = RoundedCornerShape(12.dp),
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
                        "변경하기",
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
