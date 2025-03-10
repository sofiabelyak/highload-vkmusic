# VK музыка

## 1. Тема и целевая аудитория

VK музыка - российский стриминговый сервис компании VK, объединяющий музыкальную платформу «ВКонтакте» и отдельное приложение для прослушивания музыки.

### Функционал MVP

1. Аутентификация пользователей
2. Поиск и нахождение музыки
3. Прослушивание и воспроизведение музыки
4. Добавление треков в плейлист
5. Создание и редактирование плейлистов
6. История прослушиваний пользователя

### Целевая аудитория

#### Анализ трафика и вовлеченности
* MAU - **42млн** [^1]
* DAU - **5-10млн** [^1]
* Средняя продолжительность сессии - **00:02:17** [^2]
* Среднее количество страниц на посещение - **1,90** [^2]
* Среднее количество новых пользователей в месяц - **1 млн** [^2]

#### Веб-трафик по странам

[![Traffic by Country](img/Traffic_by_Country.jpg)](https://www.similarweb.com/ru/website/music.vk.com) [^2]

#### Демографические показатели

Общая характеристика аудитории (гендер и возраст) [^2]

[![Demographic Indicators](img/Website_Traffic.jpg)](https://www.similarweb.com/ru/website/music.vk.com) 


## 2. Расчет нагрузки

### Продуктовые метрики

| Метрика                  | Значение         | Источник/Формула                              |
|--------------------------|------------------|-----------------------------------------------|
| **MAU**                  | 42M             | [Пресс-релиз VK](RUS_Press_Release_9M_2024.pdf) |
| **DAU**                  | 5-10M           | [Пресс-релиз VK](RUS_Press_Release_9M_2024.pdf) |
| **Хранилище:**           |                  |                                               |
| - Треки/пользователь             | 100              | Среднее количество треков в библиотеке пользователя по анализу аналоговых источников [Анализ аналогичного приложения. Пользователи Spotify создают в среднем 3-5 плейлистов, каждый из которых содержит 20-30 треков](https://newsroom.spotify.com/company-info/)                               |
| - Метаданные/пользователь        | 0.01 ГБ        | `100 треков × 0.1 МБ` [Расчет](#meta)                              |
| **Действия/день:**       |                  |                                               |
| - Поиск                  | 3-5             | [На основе анализа поведения пользователей Spotify, среднее количество поисковых действий оценивается в 3-5 раз в день](https://www.vox.com/2014/8/18/6003271/why-are-songs-3-minutes-long)                               |
| - Прослушивание          | 12 треков        | [Расчет](#audio-calc)                         |
| - Добавление в плейлист  | 1             | `1 добавление/12 прослушиваний`                      |

---

#### Метаданные трека {#meta}
| Компонент              | Размер на 1 трек | Размер на 100 треков |
|------------------------|------------------|----------------------|
| **ID трека**           | 16 B             | 1.6 КБ               |
| **Текстовые данные**   | 400 B            | 40 КБ                |
| **Обложка**            | 80 КБ            | 8 МБ                 |
| **Теги**               | 100 B            | 10 КБ                |
| **Дата добавления**    | 8 B              | 0.8 КБ               |
| **Дополнительные данные** | 19.5 КБ       | 1.95 МБ              |
| **Итого**              | **100 КБ (0.1 МБ)** | **10 МБ (0.01 ГБ)** |

#### Среднее количество прослушивания треков в день {#audio-calc}
1. `3-5 сессий × 2:17 мин = 6.8-11.4 мин/день`  
2. `(6.8 мин ÷ 4 мин/трек) + (11.4 ÷ 3) = 1.7(минимум)-3.8(максимум) треков`  
3. **С переключениями треков**: ` в среднем 12 треков/день, они включают повторные прослушивания и переключения между треками.`

## Технические метрики

### Хранилище
| Тип данных              | Единицы      | Объем (ТБ) | Формула расчета                     |
|-------------------------|--------------|------------|--------------------------------------|
| **Аудиофайлы**          | 50 млн треков| 250        | `5 МБ/трек × 50M`                   |
| **Метаданные треков**   | 50 млн запис.| 5          | `0.1 МБ/трек × 50M`                 |
| **Профили пользователей**| 42 млн аккаунтов | 4.2    | `100 КБ/пользователь × 42M`                 |

---

### Сетевые показатели

#### Пиковая нагрузка (Гбит/с)
| Тип трафика       | Значение | Формула                                |
|--------------------|----------|----------------------------------------|
| Аудиопотоки       | 128      | `1M потоков × 128 кбит/с`              |
| API-запросы       | 0.08       | `10k RPS × 1 КБ/запрос × 8 бит`        |

**Пояснения:**  
- **1M потоков**: 10% от DAU (10M) одновременно слушают музыку.  
- **128 кбит/с**: [Битрейт аудио для среднего качества ](https://support.spotify.com/md-ru/article/high-quality-streaming/).  
- **10k RPS**: Пиковые запросы к API (поиск, плейлисты, аутентификация).

#### Суточный трафик (ТБ/день)
| Тип трафика       | Значение | Формула                                |
|--------------------|----------|----------------------------------------|
| Аудио             | 440     | `10M DAU × 12 треков × 3.66 МБ`           |
| API               | 1        | `100M запросов × 10 КБ`                |

---

**Пояснения:**  
- **12 треков/день**: Из продуктовых метрик (прослушивание).  
- **4 МБ/трек**: Размер трека = 128 бит/с × 240 с = 30,720 бит  
Размер (МБ) = 30,720 / 8 / 1,048,576 ≈ 3.66 МБ  

### Производительность (RPS)

Средний RPS = Общее кол-во запросов в день / 86,400  
Пиковый RPS = Средний RPS × 2 (коэффициент пиковой нагрузки)

| Метод                     | Средний | Пиковый  | Формула расчета              |
|---------------------------|---------|----------|-------------------------------|
| **Поиск**                | 580     | 1,160    | `50M/86400 × 2`             |
| **Воспроизведение**      | 1,388   | 2,777   | `120M/86400 × 2`            |
| **Добавление в плейлист**| 116     | 232    | `10M/86400 × 2`             |
| **Аутентификация**       | 116     | 232    | `10M/86400 × 2`             |
| **Управление плейлистами**| 17    | 34      | `1.43M/86400 × 2`              |
| **Запись истории**       | 1,388  | 2,777   | `120M/86400 × 2`            |

---

**Пояснения:**  
- **Поиск (50M/день)**:  
  `10M DAU × 5 запросов/день` (аналогия с Spotify).  
- **Воспроизведение (120M/день)**:  
  `10M DAU × 12 треков/день` (из продуктовых метрик).  
- **Добавление в плейлист (10M/день)**:  
  `10M DAU × 1 добавление/день (1 сохранение на 12 прослушиваний).  
- **Аутентификация (10M/день)**:  
  Каждый DAU выполняет 1 вход в день.  
- **Управление плейлистами (6M/день)**:  
  `10M DAU / 7 дней` (пользователи редактируют плейлисты в среднем раз в неделю).  
- **Запись истории (120M/день)**:  
  Сохранение каждого прослушивания: `10M DAU × 12 треков`.  

**Примечания:**  
1. **Коэффициент ×2 для пика** — обоснован географическим распределением пользователей и разницей в часовых поясах. Сервис охватывает территорию СНГ, где пользователи находятся в разных часовых поясах (от UTC+2 до UTC+12). Пиковая нагрузка распределяется во времени: когда в одном регионе вечер (пик активности), в другом — утро или день (низкая активность). Таким образом, пиковая нагрузка в целом по системе не превышает [2-кратного значения](https://habr.com/en/companies/dcmiran/articles/496542/) от средней.

2. **86,400** — количество секунд в сутках (`24 × 60 × 60`).  

## 3. Глобальная балансировка нагрузки

### 1. Функциональное разбиение по доменам

Для VK Музыки функциональное разбиение сосредоточено на основном домене:

**Основной домен**: music.vk.com – лендинг и веб-интерфейс.

**API**: api.music.vk.com – аутентификация, поиск, плейлисты.

**Стриминг**: stream.music.vk.com – аудиопотоки.

**Статика**: cdn.music.vk.com – обложки, метаданные.

### 2. Обоснование расположения ДЦ (влияние на продуктовые метрики)

#### Основные регионы пользователей

На основе данных о трафике, основными регионами, где сосредоточена аудитория VK Музыки, являются:
- **Россия**: 48.47% трафика
- **Беларусь**: 15.09% трафика
- **Казахстан**: 14.18% трафика
- **Молдова**: 8.68% трафика

Эти четыре регион генерируют более 90% всего трафика сервиса. Это значит, что именно здесь сосредоточено большинство пользователей, и здесь нужно размещать ДЦ.


#### Расчет количества серверов

1. **Пиковая нагрузка**:  
   - **Аудиопотоки**: 128 Гбит/с.  
   - **API-запросы**: 10k RPS.  

2. **Мощность одного сервера**:  
   - Сервер средней мощности (8 vCPU, 32GB RAM) может выдавать примерно 300 Мбит/с.

3. **Общее количество серверов**:  
   - Пиковое потребление в сутки: 128 Гбит/с.  
   - Количество серверов:  
     - 1 Гбит = 1024 Мбит, поэтому 128 Гбит/с = 128 × 1024 = 131072 Мбит/с.  
     - Мощность одного сервера: 300 Мбит/с.  
     - Количество серверов = 131072 Мбит/с / 300 Мбит/с ≈ 437 серверов.  
     - С запасом х2: 437 × 2 = 874 сервера.

#### Распределение серверов по ДЦ

Распределим серверы по ДЦ в соответствии с количеством пользователей в каждом регионе:


| Регион       | Доля трафика | Количество серверов | Расположение ДЦ         |
|--------------|--------------|---------------------|-------------------------|
| **Россия**   | 48.47%       | 424                 | Москва, Санкт-Петербург, Екатеринбург, Новосибирск |
| **Беларусь** | 15.09%       | 132                 | Минск                   |
| **Казахстан**| 14.18%       | 124                 | Алматы                  |
| **Молдова**  | 11.8%        | 103                  | Кишинев                 |
| **Другие**   | 10.46%       | 91                  | Резервные ДЦ            |


---

#### Примечание 
  1. **Россия**:
      - **Москва и Санкт-Петербург**: Крупнейшие города с высокой концентрацией пользователей.
      - **Екатеринбург**: Ключевой город на Урале.
      - **Новосибирск**: Крупный город Сибири.
  2. **Беларусь**: **Минск** (15.09% трафика).
  3. **Казахстан**: **Алматы** (14.18% трафика).
  4. **Молдова**: **Кишинев** (11.8% трафика).

---
## 3. Глобальная балансировка нагрузки

### 1. Функциональное разбиение по доменам

Для VK Музыки функциональное разбиение сосредоточено на основном домене:

**Основной домен**: music.vk.com – лендинг и веб-интерфейс.

**API**: api.music.vk.com – аутентификация, поиск, плейлисты.

**Стриминг**: stream.music.vk.com – аудиопотоки.

**Статика**: cdn.music.vk.com – обложки, метаданные.

### 2. Обоснование расположения ДЦ (влияние на продуктовые метрики)

#### Основные регионы пользователей

На основе данных о трафике, основными регионами, где сосредоточена аудитория VK Музыки, являются:
- **Россия**: 48.47% трафика
- **Беларусь**: 15.09% трафика
- **Казахстан**: 14.18% трафика
- **Молдова**: 8.68% трафика

Эти четыре региона генерируют более 90% всего трафика сервиса. Это значит, что именно здесь сосредоточено большинство пользователей, и здесь нужно размещать ДЦ.

#### Расчет количества серверов

1. **Пиковая нагрузка**:  
   - **Аудиопотоки**: 128 Гбит/с.  
   - **API-запросы**: 10k RPS.  

2. **Мощность одного сервера**:  
   - Сервер средней мощности (8 vCPU, 32GB RAM) может выдавать примерно 300 Мбит/с.

3. **Общее количество серверов**:  
   - Пиковое потребление в сутки: 128 Гбит/с.  
   - Количество серверов:  
     - 1 Гбит = 1024 Мбит, поэтому 128 Гбит/с = 128 × 1024 = 131072 Мбит/с.  
     - Мощность одного сервера: 300 Мбит/с.  
     - Количество серверов = 131072 Мбит/с / 300 Мбит/с ≈ 437 серверов.  
     - С запасом х2: 437 × 2 = 874 сервера.

#### Распределение серверов по ДЦ

Распределим серверы по ДЦ в соответствии с количеством пользователей в каждом регионе. Резервные ДЦ включены в общее распределение:

| Регион       | Доля трафика | Количество серверов | Расположение ДЦ         | Тип ДЦ       |
|--------------|--------------|---------------------|-------------------------|--------------|
| **Россия**   | 48.47%       | 424                 | Москва, Екатеринбург, Новосибирск | Основной     |
| **Беларусь** | 15.09%       | 132                 | Минск                   | Основной     |
| **Казахстан**| 14.18%       | 124                 | Алматы                  | Основной     |
| **Молдова**  | 8.68%        | 76                  | Кишинев                 | Основной     |
| **Резервные ДЦ** | 13.58%    | 118                 | Москва, Екатеринбург, Новосибирск, Минск, Алматы, Кишинев | Резервный    |

---

#### Примечание 
1. **Россия**:
   - **Москва**: Центральный регион с наибольшей концентрацией пользователей.
   - **Екатеринбург**: Ключевой город на Урале.
   - **Новосибирск**: Крупный город Сибири.
2. **Беларусь**: **Минск** (15.09% трафика).
3. **Казахстан**: **Алматы** (14.18% трафика).
4. **Молдова**: **Кишинев** (8.68% трафика).
5. **Резервные ДЦ**: Расположены в тех же регионах, что и основные ДЦ, для обеспечения отказоустойчивости.

---

### 3. Распределение запросов по типам и ДЦ

На основе данных из раздела "Расчет нагрузки":

#### Распределение запросов по типам

| Тип запроса           | Количество запросов/день | Распределение по ДЦ  |
|------------------------|--------------------------|------------------------------|
| **Поиск**             | 50M                     | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ |
| **Воспроизведение**   | 120M                    | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ |
| **Добавление в плейлист** | 10M                  | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ|
| **Аутентификация**    | 10M                     | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ |
| **Управление плейлистами** | 6M               | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ |
| **Запись истории**    | 120M                    | 48.47% — Россия, 15.09% — Беларусь, 14.18% — Казахстан, 8.68% — Молдова, 13.58% — Резервные ДЦ |

---

#### Распределение запросов по ДЦ

| Регион       | Доля трафика | Распределение запросов | Расположение ДЦ         | Тип ДЦ       |
|--------------|--------------|------------------------|-------------------------|--------------|
| **Россия**   | 48.47%       | 48.47%                 | Москва, Екатеринбург, Новосибирск | Основной     |
| **Беларусь** | 15.09%       | 15.09%                 | Минск                   | Основной     |
| **Казахстан**| 14.18%       | 14.18%                 | Алматы                  | Основной     |
| **Молдова**  | 8.68%        | 8.68%                  | Кишинев                 | Основной     |
| **Резервные ДЦ** | 13.58%    | 13.58%                 | Москва, Екатеринбург, Новосибирск, Минск, Алматы, Кишинев | Резервный    |

---

#### Расчёты для каждого ДЦ

| Тип запроса           | Москва (30%) | Екатеринбург (10%) | Новосибирск (8.47%) | Минск (15.09%) | Алматы (14.18%) | Кишинев (8.68%) | Резервные ДЦ (13.58%) |
|------------------------|--------------|--------------------|---------------------|----------------|-----------------|-----------------|-----------------------|
| **Поиск**             | 15M/день     | 5M/день            | 4.235M/день         | 7.545M/день    | 7.09M/день      | 4.34M/день      | 6.79M/день            |
| **Воспроизведение**   | 36M/день     | 12M/день           | 10.164M/день        | 18.108M/день   | 17.016M/день    | 10.416M/день    | 16.296M/день          |
| **Добавление в плейлист** | 3M/день  | 1M/день            | 0.847M/день         | 1.509M/день    | 1.418M/день     | 0.868M/день     | 1.358M/день           |
| **Аутентификация**    | 3M/день      | 1M/день            | 0.847M/день         | 1.509M/день    | 1.418M/день     | 0.868M/день     | 1.358M/день           |
| **Управление плейлистами** | 1.8M/день | 0.6M/день          | 0.508M/день         | 0.905M/день    | 0.851M/день     | 0.521M/день     | 0.815M/день           |
| **Запись истории**    | 36M/день     | 12M/день           | 10.164M/день        | 18.108M/день   | 17.016M/день    | 10.416M/день    | 16.296M/день          |

### 4. Схема DNS балансировки

Каждый ДЦ равномерно обрабатывает все типы запросов. В случае отказа одного ДЦ, запросы перенаправляются на резервные ДЦ. Логичным будет использование схемы балансировки Geo-based DNS, которая использует географическое распределение для минимизации задержек и обеспечивает отказоустойчивость за счёт резервных ДЦ.

1. **Москва (ДЦ 1)** → Резерв: Екатеринбург (ДЦ 2) → Резерв: Новосибирск (ДЦ 3)
2. **Екатеринбург (ДЦ 2)** → Резерв: Москва (ДЦ 1) → Резерв: Новосибирск (ДЦ 3)
3. **Новосибирск (ДЦ 3)** → Резерв: Москва (ДЦ 1) → Резерв: Екатеринбург (ДЦ 2)
4. **Минск (ДЦ 4)** → Резерв: Кишинев (ДЦ 6) → Резерв: Резервные ДЦ
5. **Алматы (ДЦ 5)** → Резерв: Новосибирск (ДЦ 3) → Резерв: Екатеринбург (ДЦ 2)
6. **Кишинев (ДЦ 6)** → Резерв: Минск (ДЦ 4) → Резерв: Резервные ДЦ

Каждый ДЦ имеет два уровня резервов. Если основной ДЦ падает, запросы перекидываются на первый резерв. Если и резерв падает, то на второй.

### 5. Схема Anycast балансировки
Для глобального стриминга лучше использовать BGP Anycast. DNS-балансировки недостаточно. Anycast через BGP мгновенно переключает трафик при падении ДЦ, а DNS зависит от TTL кешей. Anycast автоматически направляет запросы к ближайшему ДЦ на уровне сетевой маршрутизации, уменьшая задержки. 

### 6. Механизм регулировки трафика между ДЦ

Для VK Музыки используется гибридный подход, сочетающий DNS и Anycast балансировку. 

1. **Географическая балансировка (GeoDNS)**:  
   - Пользователи подключаются к ближайшему ДЦ на основе их IP-адреса.  

2. **Резервирование при сбоях**:  
   - Если ДЦ падает, DNS и Anycast перенаправляют трафик на резервы.

| Тип трафика              | Метод регулировки       | 
|--------------------------|-------------------------|
| **Поиск**                | Anycast                 | 
| **Аудиопотоки**          | GeoDNS  + резервы       | 
| **Аутентификация**       | GeoDNS                 | 
| **Управление плейлистами** | GeoDNS  |
| **Добавление в плейлист** | GeoDNS                 | 
| **Запись истории**       | GeoDNS                  |

---

## 4. Локальная балансировка нагрузки

Для музыкального приложения VK Музыка локальная балансировка будет организована следующим образом:

### 1. Схемы балансировки

### Аудиопотоки (stream.music.vk.com)
- Балансировка: на уровне L4 (TCP/UDP) с использованием  NGINX.
- Алгоритм балансировки: Least Connections для равномерного распределения нагрузки, чтобы обеспечить стабильную и высококачественную работу сервиса.

### API-запросы (api.music.vk.com)
- Балансировка: на уровне L7 (HTTP/HTTPS) с использованием NGINX.
- Алгоритм балансировки: Round Robin с учетом весов серверов, чтобы эффективно использовать ресурсы и улучшить общее время отклика.

### Межсервисное взаимодействие
- Балансировка: использование Sidecar proxy для балансировки между микросервисами.
- Интеграция: с Service discovery (Kubernetes).

### Статика (cdn.music.vk.com)
- Балансировка: на уровне L7 (HTTP/HTTPS) с использованием NGINX.
- Кэширование: статики на балансировщике для снижения нагрузки на backend.

### 2. Схема отказоустойчивости

Для обеспечения отказоустойчивости внутри каждого ДЦ необходимо:

### Резервирование серверов:
Каждый сервис должен иметь как минимум два экземпляра.
В случае сбоя одного сервера, балансировщик автоматически перенаправляет запросы на резервные серверы.
### Health checks:
Балансировщики (NGINX) должны регулярно проверять доступность серверов.

### Автоматическое восстановление:
Использование Kubernetes для автоматического перезапуска упавших сервисов.

### Географическое резервирование:
Если ДЦ полностью выходит из строя, трафик перенаправляется на резервные ДЦ через GeoDNS или Anycast.

### 3. Расчет нагрузки по терминации SSL
Терминация SSL на балансировщике снижает нагрузку на серверы, но требует значительных ресурсов на самом балансировщике. Рассчитаем нагрузку для API-запросов:

RPS: 10,000.
Размер ключа SSL: 2048 бит
Ресурсы на одно соединение: ~1 ms CPU на одно SSL-рукопожатие.
Общая нагрузка:
Для 10,000 RPS потребуется примерно 10 секунд CPU времени на обработку SSL-рукопожатий.
С учетом параллельной обработки, потребуется несколько ядер CPU для терминации SSL.


Таким образом, локальная балансировка нагрузки должна быть организована с учетом специфики каждого типа запросов:
- L4 для аудиопотоков (низкая задержка, высокая пропускная способность).
- L7 для API и статики (глубокая маршрутизация, терминация SSL).
- Sidecar proxy для межсервисного взаимодействия (отказоустойчивость, автоматическое обнаружение сервисов).

## Список источников

https://corp.vkcdn.ru/media/files/RUS_Press_Release_9M_2024.pdf

https://www.similarweb.com/ru/website/music.vk.com

https://support.spotify.com/md-ru/article/high-quality-streaming/

https://habr.com/en/companies/dcmiran/articles/496542/

https://newsroom.spotify.com/company-info/

https://www.vox.com/2014/8/18/6003271/why-are-songs-3-minutes-long
